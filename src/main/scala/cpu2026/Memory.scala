package cpu2026

import chisel3._
import chisel3.util._

class StoreRequest extends Bundle {
  val addr  = UInt(32.W)
  val data  = UInt(32.W)
  val strobe = UInt(4.W)
  val isExit = Bool()
}

class WriteResponse extends Bundle {
  val bresp = UInt(2.W)
  val isExit = Bool()
}

/** Course-provided synchronous single-port SRAM. */
class Sram(depth: Int, width: Int, writeGranularity: Int) extends ExtModule(
  Map(
    "DEPTH" -> IntParam(depth),
    "WIDTH" -> IntParam(width),
    "WRITE_GRANULARITY" -> IntParam(writeGranularity)
  )
) {
  require(depth >= 1 && depth <= 1048576)
  require(width >= 1 && width <= 4096)
  require(depth.toLong * width <= 16777216L)
  require(writeGranularity >= 1 && writeGranularity <= width)
  require(width % writeGranularity == 0)

  override def desiredName: String = "sram_fakeram"

  val clk = IO(Input(Clock()))
  val en = IO(Input(Bool()))
  val we = IO(Input(Bool()))
  val wmask = IO(Input(UInt((width / writeGranularity).W)))
  val addr = IO(Input(UInt(math.max(1, log2Ceil(depth)).W)))
  val wdata = IO(Input(UInt(width.W)))
  val rdata = IO(Output(UInt(width.W)))
}

/** Serial AXI4-Lite read engine shared by the instruction and data caches.
  *
  * AXI preserves the order of read responses. Keeping one read in flight avoids
  * response identifiers while still allowing I and D requests from independent
  * clients.
  */
class AxiReadArbiter extends Module {
  val iReq  = IO(Flipped(Decoupled(UInt(32.W))))
  val iResp = IO(Valid(UInt(32.W)))
  val dReq  = IO(Flipped(Decoupled(UInt(32.W))))
  val dResp = IO(Valid(UInt(32.W)))

  val araddr  = IO(Output(UInt(32.W)))
  val arvalid = IO(Output(Bool()))
  val arready = IO(Input(Bool()))
  val rdata   = IO(Input(UInt(32.W)))
  val rvalid  = IO(Input(Bool()))
  val rready  = IO(Output(Bool()))

  private val idle :: address :: data :: Nil = Enum(3)
  val state = RegInit(idle)
  val lastWasI = RegInit(false.B)
  val addressReg = Reg(UInt(32.W))
  val selectedI = Reg(Bool())

  iResp := 0.U.asTypeOf(Valid(UInt(32.W)))
  dResp := 0.U.asTypeOf(Valid(UInt(32.W)))

  private val chooseI = iReq.valid && (!lastWasI || !dReq.valid)
  iReq.ready := state === idle && chooseI
  dReq.ready := state === idle && !chooseI && dReq.valid

  araddr := addressReg
  arvalid := state === address
  rready := state === data

  switch(state) {
    is(idle) {
      when(iReq.valid || dReq.valid) {
        selectedI := chooseI
        lastWasI := chooseI
        addressReg := Mux(chooseI, iReq.bits, dReq.bits) & "hfffffffc".U
        state := address
      }
    }
    is(address) {
      when(arready) { state := data }
    }
    is(data) {
      when(rvalid) {
        when(selectedI) {
          iResp.valid := true.B
          iResp.bits := rdata
        }.otherwise {
          dResp.valid := true.B
          dResp.bits := rdata
        }
        state := idle
      }
    }
  }
}

/** Serial AXI4-Lite write engine used for committed stores. */
class AxiWriteEngine extends Module {
  val req     = IO(Flipped(Decoupled(new StoreRequest)))
  val resp    = IO(Valid(new WriteResponse))
  val idle    = IO(Output(Bool()))

  val awaddr  = IO(Output(UInt(32.W)))
  val awvalid = IO(Output(Bool()))
  val awready = IO(Input(Bool()))
  val wdata   = IO(Output(UInt(32.W)))
  val wstrb   = IO(Output(UInt(4.W)))
  val wvalid  = IO(Output(Bool()))
  val wready  = IO(Input(Bool()))
  val bresp   = IO(Input(UInt(2.W)))
  val bvalid  = IO(Input(Bool()))
  val bready  = IO(Output(Bool()))

  private val idleState :: issueState :: responseState :: Nil = Enum(3)
  val state = RegInit(idleState)
  val request = Reg(new StoreRequest)
  val awDone = RegInit(false.B)
  val wDone = RegInit(false.B)

  req.ready := state === idleState
  resp := 0.U.asTypeOf(Valid(new WriteResponse))
  idle := state === idleState

  awaddr := request.addr
  awvalid := state === issueState && !awDone
  wdata := request.data
  wstrb := request.strobe
  wvalid := state === issueState && !wDone
  bready := state === responseState

  switch(state) {
    is(idleState) {
      when(req.fire) {
        request := req.bits
        awDone := false.B
        wDone := false.B
        state := issueState
      }
    }
    is(issueState) {
      when(awvalid && awready) { awDone := true.B }
      when(wvalid && wready) { wDone := true.B }
      when((awDone || (awvalid && awready)) && (wDone || (wvalid && wready))) {
        state := responseState
      }
    }
    is(responseState) {
      when(bvalid) {
        resp.valid := true.B
        resp.bits.bresp := bresp
        resp.bits.isExit := request.isExit
        state := idleState
      }
    }
  }
}

class CacheLine(tagWidth: Int, lineWords: Int) extends Bundle {
  val valid = Bool()
  val tag   = UInt(tagWidth.W)
  val data  = Vec(lineWords, UInt(32.W))
}

class InstructionCache(config: CpuConfig, fetchWidth: Int) extends Module {
  require(config.cacheLineBytes % 4 == 0)
  require(fetchWidth >= 1 && fetchWidth <= 4)

  val pc = IO(Input(UInt(32.W)))
  val instrs = IO(Output(Vec(fetchWidth, UInt(32.W))))
  val count = IO(Output(UInt(log2Ceil(fetchWidth + 1).W)))
  val flush = IO(Input(Bool()))

  val memReq  = IO(Decoupled(UInt(32.W)))
  val memResp = IO(Input(Valid(UInt(32.W))))

  private val lineWords = config.cacheLineBytes / 4
  private val sets = config.icacheBytes / (config.cacheLineBytes * config.cacheWays)
  private val ways = config.cacheWays
  require(sets >= 1 && sets <= (1 << 20))
  private val indexBits = log2Ceil(sets)
  private val offsetBits = log2Ceil(config.cacheLineBytes)
  private val tagBits = 32 - indexBits - offsetBits

  val lines = Reg(Vec(sets, Vec(ways, new CacheLine(tagBits, lineWords))))
  val valid = RegInit(VecInit(Seq.fill(sets)(VecInit(Seq.fill(ways)(false.B)))))

  private val idle :: fillAddrState :: fillDataState :: Nil = Enum(3)
  val state = RegInit(idle)
  val fillAddress = Reg(UInt(32.W))
  val fillTag = Reg(UInt(tagBits.W))
  val fillSet = Reg(UInt(indexBits.W))
  val fillWay = Reg(UInt(log2Ceil(ways).W))
  val fillWord = Reg(UInt(log2Ceil(lineWords).W))

  private val requestAddrs = VecInit((0 until fetchWidth).map { lane =>
    pc + (lane * 4).U
  })
  private val setIndexes = VecInit(requestAddrs.map { address =>
    (address >> offsetBits)(indexBits - 1, 0)
  })
  private val tags = VecInit(requestAddrs.map { address =>
    if (tagBits == 0) 0.U else address >> (offsetBits + indexBits)
  })
  private val wordIndexes = VecInit(requestAddrs.map { address =>
    (address >> 2)(log2Ceil(lineWords) - 1, 0)
  })
  private val hitVectors = VecInit((0 until fetchWidth).map { lane =>
    VecInit((0 until ways).map { way =>
      valid(setIndexes(lane))(way) && lines(setIndexes(lane))(way).tag === tags(lane)
    })
  })
  private val laneHits = VecInit(hitVectors.map(_.asUInt.orR))
  private val laneData = VecInit((0 until fetchWidth).map { lane =>
    Mux1H(
      hitVectors(lane),
      VecInit((0 until ways).map { way =>
        lines(setIndexes(lane))(way).data(wordIndexes(lane))
      })
    )
  })
  private val prefixHits = VecInit((0 until fetchWidth).map { lane =>
    (0 to lane).map(index => laneHits(index)).reduce(_ && _)
  })
  private val validLanes = Mux(
    state === idle,
    prefixHits,
    VecInit(Seq.fill(fetchWidth)(false.B))
  )
  count := PopCount(validLanes)
  instrs := laneData

  private val missLanes = VecInit((0 until fetchWidth).map(lane => !laneHits(lane)))
  private val firstMiss = PriorityEncoder(missLanes)
  private val firstMissAddress = requestAddrs(firstMiss)
  private val lineBase = Cat(firstMissAddress(31, offsetBits), 0.U(offsetBits.W))
  private val emptyVec = VecInit((0 until ways).map { way =>
    !valid(setIndexes(firstMiss))(way)
  })
  private val victim = Mux(emptyVec.asUInt.orR, PriorityEncoder(emptyVec), 0.U)

  memReq.valid := state === fillAddrState
  memReq.bits := fillAddress

  when(flush) {
    state := idle
  }

  when(state === idle && missLanes.asUInt.orR) {
    fillAddress := lineBase
    fillTag := tags(firstMiss)
    fillSet := setIndexes(firstMiss)
    fillWay := victim
    fillWord := 0.U
    state := fillAddrState
  }

  when(state === fillAddrState && memReq.ready) {
    state := fillDataState
  }

  when(state === fillDataState && memResp.valid) {
    lines(fillSet)(fillWay).data(fillWord) := memResp.bits
    when(fillWord === (lineWords - 1).U) {
      lines(fillSet)(fillWay).tag := fillTag
      valid(fillSet)(fillWay) := true.B
      state := idle
    }.otherwise {
      fillAddress := fillAddress + 4.U
      fillWord := fillWord + 1.U
      state := fillAddrState
    }
  }
}

class DataCache(config: CpuConfig) extends Module {
  require(config.cacheLineBytes % 4 == 0)
  private val debugTrace = sys.env.get("CPU_DEBUG").contains("1")

  val loadReq  = IO(Flipped(Decoupled(UInt(32.W))))
  val loadResp = IO(Valid(UInt(32.W)))
  val storeReq = IO(Flipped(Decoupled(new StoreRequest)))
  val storeResp = IO(Valid(UInt(2.W)))

  val memReq  = IO(Decoupled(UInt(32.W)))
  val memResp = IO(Input(Valid(UInt(32.W))))
  val writeReq  = IO(Decoupled(new StoreRequest))
  val writeResp = IO(Input(Valid(new WriteResponse)))
  val writeIdle = IO(Input(Bool()))

  private val lineWords = config.cacheLineBytes / 4
  private val sets = config.dcacheBytes / (config.cacheLineBytes * config.cacheWays)
  private val ways = config.cacheWays
  require(sets >= 1 && sets <= (1 << 20))
  private val indexBits = log2Ceil(sets)
  private val offsetBits = log2Ceil(config.cacheLineBytes)
  private val tagBits = 32 - indexBits - offsetBits
  private val lineBits = lineWords * 32
  private val lineCount = sets * ways

  val lineSram = Module(new Sram(lineCount, lineBits, lineBits))
  val valid = RegInit(VecInit(Seq.fill(sets)(VecInit(Seq.fill(ways)(false.B)))))
  val tags = Reg(Vec(sets, Vec(ways, UInt(tagBits.W))))

  private val states = Enum(10)
  private val idle = states(0)
  private val readLoad = states(1)
  private val readLoadWait = states(2)
  private val readStore = states(3)
  private val readStoreWait = states(4)
  private val storeWrite = states(5)
  private val fillDrain = states(6)
  private val fillAddrState = states(7)
  private val fillDataState = states(8)
  private val fillWrite = states(9)
  val state = RegInit(idle)
  val fillAddress = Reg(UInt(32.W))
  val fillTag = Reg(UInt(tagBits.W))
  val fillSet = Reg(UInt(indexBits.W))
  val fillWay = Reg(UInt(log2Ceil(ways).W))
  val fillWord = Reg(UInt(log2Ceil(lineWords).W))
  val fillLineData = Reg(Vec(lineWords, UInt(32.W)))
  val requestedIndex = Reg(UInt(log2Ceil(lineWords).W))
  val requestedData = Reg(UInt(32.W))
  val pendingLoadWord = Reg(UInt(log2Ceil(lineWords).W))
  val pendingLoadSramAddr = Reg(UInt(math.max(1, log2Ceil(lineCount)).W))
  val pendingStore = Reg(new StoreRequest)
  val pendingStoreWord = Reg(UInt(log2Ceil(lineWords).W))
  val pendingStoreSramAddr = Reg(UInt(math.max(1, log2Ceil(lineCount)).W))
  val pendingStoreLine = Reg(Vec(lineWords, UInt(32.W)))
  val loadResponseValid = RegInit(false.B)
  val loadResponseData = Reg(UInt(32.W))
  val storeResponse = RegInit(0.U.asTypeOf(Valid(UInt(2.W))))
  val writeQueue = Module(new Queue(new StoreRequest, 8))

  // Loads have priority so the core can decide whether to issue a memory uop
  // without creating a valid/ready path through a pending committed store.
  loadReq.ready := state === idle && !loadResponseValid
  storeReq.ready := state === idle && !loadReq.valid && !loadResponseValid &&
    writeQueue.io.enq.ready
  loadResp := 0.U.asTypeOf(Valid(UInt(32.W)))
  storeResp := storeResponse

  private val loadAddress = loadReq.bits
  private val loadSet = (loadAddress >> offsetBits)(indexBits - 1, 0)
  private val loadTag = if (tagBits == 0) 0.U else loadAddress >> (offsetBits + indexBits)
  private val loadWord = (loadAddress >> 2)(log2Ceil(lineWords) - 1, 0)
  private val loadHitVec = VecInit((0 until ways).map { way =>
    valid(loadSet)(way) && tags(loadSet)(way) === loadTag
  })
  private val loadHit = loadHitVec.asUInt.orR

  private val storeAddress = storeReq.bits.addr
  private val storeSet = (storeAddress >> offsetBits)(indexBits - 1, 0)
  private val storeTag = if (tagBits == 0) 0.U else storeAddress >> (offsetBits + indexBits)
  private val storeWord = (storeAddress >> 2)(log2Ceil(lineWords) - 1, 0)
  private val storeHitVec = VecInit((0 until ways).map { way =>
    valid(storeSet)(way) && tags(storeSet)(way) === storeTag
  })

  private val emptyVec = VecInit((0 until ways).map(way => !valid(loadSet)(way)))
  private val victim = Mux(emptyVec.asUInt.orR, PriorityEncoder(emptyVec), 0.U)
  private val lineBase = Cat(loadAddress(31, offsetBits), 0.U(offsetBits.W))

  memReq.valid := state === fillAddrState
  memReq.bits := fillAddress
  writeQueue.io.enq.valid := storeReq.fire
  writeQueue.io.enq.bits := storeReq.bits
  writeReq.valid := writeQueue.io.deq.valid
  writeReq.bits := writeQueue.io.deq.bits
  writeQueue.io.deq.ready := writeReq.ready

  val readWords = VecInit((0 until lineWords).map { word =>
    lineSram.rdata(word * 32 + 31, word * 32)
  })
  lineSram.clk := clock
  lineSram.en := state === readLoad || state === readStore ||
    state === fillWrite || state === storeWrite
  lineSram.we := state === fillWrite || state === storeWrite
  lineSram.wmask := true.B
  lineSram.addr := MuxLookup(state, 0.U)(
    Seq(
      readLoad -> pendingLoadSramAddr,
      readStore -> pendingStoreSramAddr,
      storeWrite -> pendingStoreSramAddr,
      fillWrite -> (fillSet * ways.U + fillWay)
    )
  )
  lineSram.wdata := Cat((0 until lineWords).reverse.map { word =>
    Mux(
      state === storeWrite,
      pendingStoreLine(word),
      fillLineData(word)
    )
  })

  when(loadResponseValid) {
    loadResponseValid := false.B
  }
  when(storeResponse.valid) {
    storeResponse.valid := false.B
  }

  loadResp.valid := loadResponseValid
  loadResp.bits := loadResponseData

  when(loadReq.fire) {
    if (debugTrace) {
      printf(
        p"DLOAD_REQ addr=${loadAddress} hit=${loadHit} set=${loadSet} " +
          p"tag=${loadTag} word=${loadWord}\n"
      )
    }
    when(loadHit) {
      pendingLoadWord := loadWord
      pendingLoadSramAddr := loadSet * ways.U + OHToUInt(loadHitVec)
      state := readLoad
    }.otherwise {
      fillAddress := lineBase
      fillTag := loadTag
      fillSet := loadSet
      fillWay := victim
      fillWord := 0.U
      requestedIndex := loadWord
      state := Mux(
        writeQueue.io.count === 0.U && writeIdle,
        fillAddrState,
        fillDrain
      )
    }
  }

  when(state === readLoad) {
    state := readLoadWait
  }

  when(state === readLoadWait) {
    loadResponseValid := true.B
    loadResponseData := readWords(pendingLoadWord)
    if (debugTrace) {
      printf(p"DLOAD_RESP word=${pendingLoadWord} data=${readWords(pendingLoadWord)}\n")
    }
    state := idle
  }

  when(state === fillDrain && writeQueue.io.count === 0.U && writeIdle) {
    state := fillAddrState
  }

  when(state === fillAddrState && memReq.ready) {
    state := fillDataState
  }

  when(state === fillDataState && memResp.valid) {
    if (debugTrace) {
      printf(
        p"DFILL addr=${fillAddress} word=${fillWord} data=${memResp.bits}\n"
      )
    }
    fillLineData(fillWord) := memResp.bits
    when(fillWord === requestedIndex) {
      requestedData := memResp.bits
    }
    when(fillWord === (lineWords - 1).U) {
      tags(fillSet)(fillWay) := fillTag
      valid(fillSet)(fillWay) := true.B
      state := fillWrite
    }.otherwise {
      fillAddress := fillAddress + 4.U
      fillWord := fillWord + 1.U
      state := fillAddrState
    }
  }

  when(state === fillWrite) {
    loadResponseValid := true.B
    loadResponseData := Mux(
      requestedIndex === (lineWords - 1).U,
      fillLineData(lineWords - 1),
      requestedData
    )
    state := idle
    if (debugTrace) {
      printf(
        p"DFILL_DONE set=${fillSet} way=${fillWay} tag=${fillTag} " +
          p"requested=${requestedIndex} data=${loadResponseData}\n"
      )
    }
  }

  when(storeReq.fire) {
    pendingStore := storeReq.bits
    when(storeHitVec.asUInt.orR) {
      pendingStoreWord := storeWord
      pendingStoreSramAddr := storeSet * ways.U + OHToUInt(storeHitVec)
      state := readStore
    }.otherwise {
      when(!storeReq.bits.isExit) {
        storeResponse.valid := true.B
        storeResponse.bits := 0.U
      }
    }
  }

  when(state === readStore) {
    state := readStoreWait
  }

  when(state === readStoreWait) {
    val oldWord = readWords(pendingStoreWord)
    val merged = Cat((0 until 4).reverse.map { byte =>
      Mux(
        pendingStore.strobe(byte),
        pendingStore.data(byte * 8 + 7, byte * 8),
        oldWord(byte * 8 + 7, byte * 8)
      )
    })
    for (word <- 0 until lineWords) {
      pendingStoreLine(word) := readWords(word)
    }
    pendingStoreLine(pendingStoreWord) := merged
    state := storeWrite
  }

  when(state === storeWrite) {
    when(!pendingStore.isExit) {
      storeResponse.valid := true.B
      storeResponse.bits := 0.U
    }
    state := idle
  }

  when(writeResp.valid && writeResp.bits.isExit) {
    storeResponse.valid := true.B
    storeResponse.bits := writeResp.bits.bresp
  }
}

class MemorySubsystem(config: CpuConfig) extends Module {
  val iPc = IO(Input(UInt(32.W)))
  val iInstrs = IO(Output(Vec(config.issueWidth, UInt(32.W))))
  val iCount = IO(Output(UInt(log2Ceil(config.issueWidth + 1).W)))
  val iFlush = IO(Input(Bool()))
  val dLoadReq  = IO(Flipped(Decoupled(UInt(32.W))))
  val dLoadResp = IO(Valid(UInt(32.W)))
  val dStoreReq  = IO(Flipped(Decoupled(new StoreRequest)))
  val dStoreResp = IO(Valid(UInt(2.W)))

  val araddr  = IO(Output(UInt(32.W)))
  val arvalid = IO(Output(Bool()))
  val arready = IO(Input(Bool()))
  val rdata   = IO(Input(UInt(32.W)))
  val rvalid  = IO(Input(Bool()))
  val rready  = IO(Output(Bool()))

  val awaddr  = IO(Output(UInt(32.W)))
  val awvalid = IO(Output(Bool()))
  val awready = IO(Input(Bool()))
  val wdata   = IO(Output(UInt(32.W)))
  val wstrb   = IO(Output(UInt(4.W)))
  val wvalid  = IO(Output(Bool()))
  val wready  = IO(Input(Bool()))
  val bresp   = IO(Input(UInt(2.W)))
  val bvalid  = IO(Input(Bool()))
  val bready  = IO(Output(Bool()))

  val icache = Module(new InstructionCache(config, config.issueWidth))
  val dcache = Module(new DataCache(config))
  val reads = Module(new AxiReadArbiter)
  val writes = Module(new AxiWriteEngine)

  icache.pc := iPc
  iInstrs := icache.instrs
  iCount := icache.count
  icache.flush := iFlush
  dLoadReq <> dcache.loadReq
  dLoadResp := dcache.loadResp
  dStoreReq <> dcache.storeReq
  dStoreResp := dcache.storeResp

  reads.iReq <> icache.memReq
  icache.memResp := reads.iResp
  reads.dReq <> dcache.memReq
  dcache.memResp := reads.dResp

  writes.req <> dcache.writeReq
  dcache.writeResp := writes.resp
  dcache.writeIdle := writes.idle

  araddr := reads.araddr
  arvalid := reads.arvalid
  reads.arready := arready
  reads.rdata := rdata
  reads.rvalid := rvalid
  rready := reads.rready

  awaddr := writes.awaddr
  awvalid := writes.awvalid
  writes.awready := awready
  wdata := writes.wdata
  wstrb := writes.wstrb
  wvalid := writes.wvalid
  writes.wready := wready
  writes.bresp := bresp
  writes.bvalid := bvalid
  bready := writes.bready
}
