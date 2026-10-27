package cpu2026

import chisel3._
import chisel3.util._

class StoreRequest extends Bundle {
  val addr  = UInt(32.W)
  val data  = UInt(32.W)
  val strobe = UInt(4.W)
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
  val resp    = IO(Valid(UInt(2.W)))

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

  private val idle :: issue :: response :: Nil = Enum(3)
  val state = RegInit(idle)
  val request = Reg(new StoreRequest)
  val awDone = RegInit(false.B)
  val wDone = RegInit(false.B)

  req.ready := state === idle
  resp := 0.U.asTypeOf(Valid(UInt(2.W)))

  awaddr := request.addr
  awvalid := state === issue && !awDone
  wdata := request.data
  wstrb := request.strobe
  wvalid := state === issue && !wDone
  bready := state === response

  switch(state) {
    is(idle) {
      when(req.fire) {
        request := req.bits
        awDone := false.B
        wDone := false.B
        state := issue
      }
    }
    is(issue) {
      when(awvalid && awready) { awDone := true.B }
      when(wvalid && wready) { wDone := true.B }
      when((awDone || (awvalid && awready)) && (wDone || (wvalid && wready))) {
        state := response
      }
    }
    is(response) {
      when(bvalid) {
        resp.valid := true.B
        resp.bits := bresp
        state := idle
      }
    }
  }
}

class CacheLine(tagWidth: Int, lineWords: Int) extends Bundle {
  val valid = Bool()
  val tag   = UInt(tagWidth.W)
  val data  = Vec(lineWords, UInt(32.W))
}

class InstructionCache(config: CpuConfig) extends Module {
  require(config.cacheLineBytes % 4 == 0)

  val req  = IO(Flipped(Decoupled(UInt(32.W))))
  val resp = IO(Valid(UInt(32.W)))

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
  val requestedIndex = Reg(UInt(log2Ceil(lineWords).W))
  val requestedData = Reg(UInt(32.W))
  val responseValid = RegInit(false.B)
  val responseData = Reg(UInt(32.W))

  req.ready := state === idle && !responseValid
  resp := 0.U.asTypeOf(Valid(UInt(32.W)))

  private val request = req.bits
  private val setIndex = (request >> offsetBits)(indexBits - 1, 0)
  private val tag = if (tagBits == 0) 0.U else request >> (offsetBits + indexBits)
  private val wordIndex = (request >> 2)(log2Ceil(lineWords) - 1, 0)
  private val hitVec = VecInit((0 until ways).map { way =>
    valid(setIndex)(way) && lines(setIndex)(way).tag === tag
  })
  private val hit = hitVec.asUInt.orR
  private val hitWay = OHToUInt(hitVec)
  private val hitData = Mux1H(hitVec, VecInit((0 until ways).map { way =>
    lines(setIndex)(way).data(wordIndex)
  }))

  private val emptyVec = VecInit((0 until ways).map(way => !valid(setIndex)(way)))
  private val victim = Mux(emptyVec.asUInt.orR, PriorityEncoder(emptyVec), 0.U)
  private val lineBase = Cat(request(31, offsetBits), 0.U(offsetBits.W))

  memReq.valid := state === fillAddrState
  memReq.bits := fillAddress

  when(responseValid) {
    responseValid := false.B
  }

  resp.valid := responseValid
  resp.bits := responseData

  when(req.fire) {
    when(hit) {
      responseValid := true.B
      responseData := hitData
    }.otherwise {
      fillAddress := lineBase
      fillTag := tag
      fillSet := setIndex
      fillWay := victim
      fillWord := 0.U
      requestedIndex := wordIndex
      state := fillAddrState
    }
  }

  when(state === fillAddrState && memReq.ready) {
    state := fillDataState
  }

  when(state === fillDataState && memResp.valid) {
    lines(fillSet)(fillWay).data(fillWord) := memResp.bits
    when(fillWord === requestedIndex) {
      requestedData := memResp.bits
    }
    when(fillWord === (lineWords - 1).U) {
      lines(fillSet)(fillWay).tag := fillTag
      valid(fillSet)(fillWay) := true.B
      responseValid := true.B
      responseData := Mux(fillWord === requestedIndex, memResp.bits, requestedData)
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

  val loadReq  = IO(Flipped(Decoupled(UInt(32.W))))
  val loadResp = IO(Valid(UInt(32.W)))
  val storeReq = IO(Flipped(Decoupled(new StoreRequest)))
  val storeResp = IO(Valid(UInt(2.W)))

  val memReq  = IO(Decoupled(UInt(32.W)))
  val memResp = IO(Input(Valid(UInt(32.W))))
  val writeReq  = IO(Decoupled(new StoreRequest))
  val writeResp = IO(Input(Valid(UInt(2.W))))

  private val lineWords = config.cacheLineBytes / 4
  private val sets = config.dcacheBytes / (config.cacheLineBytes * config.cacheWays)
  private val ways = config.cacheWays
  require(sets >= 1 && sets <= (1 << 20))
  private val indexBits = log2Ceil(sets)
  private val offsetBits = log2Ceil(config.cacheLineBytes)
  private val tagBits = 32 - indexBits - offsetBits

  val lines = Reg(Vec(sets, Vec(ways, new CacheLine(tagBits, lineWords))))
  val valid = RegInit(VecInit(Seq.fill(sets)(VecInit(Seq.fill(ways)(false.B)))))

  private val idle :: fillAddrState :: fillDataState :: write :: writeWait :: Nil = Enum(5)
  val state = RegInit(idle)
  val fillAddress = Reg(UInt(32.W))
  val fillTag = Reg(UInt(tagBits.W))
  val fillSet = Reg(UInt(indexBits.W))
  val fillWay = Reg(UInt(log2Ceil(ways).W))
  val fillWord = Reg(UInt(log2Ceil(lineWords).W))
  val requestedIndex = Reg(UInt(log2Ceil(lineWords).W))
  val requestedData = Reg(UInt(32.W))
  val loadResponseValid = RegInit(false.B)
  val loadResponseData = Reg(UInt(32.W))
  val storeResponse = RegInit(0.U.asTypeOf(Valid(UInt(2.W))))
  val pendingStore = Reg(new StoreRequest)

  // Loads have priority so the core can decide whether to issue a memory uop
  // without creating a valid/ready path through a pending committed store.
  loadReq.ready := state === idle && !loadResponseValid
  storeReq.ready := state === idle && !loadReq.valid && !loadResponseValid
  loadResp := 0.U.asTypeOf(Valid(UInt(32.W)))
  storeResp := storeResponse

  private val loadAddress = loadReq.bits
  private val loadSet = (loadAddress >> offsetBits)(indexBits - 1, 0)
  private val loadTag = if (tagBits == 0) 0.U else loadAddress >> (offsetBits + indexBits)
  private val loadWord = (loadAddress >> 2)(log2Ceil(lineWords) - 1, 0)
  private val loadHitVec = VecInit((0 until ways).map { way =>
    valid(loadSet)(way) && lines(loadSet)(way).tag === loadTag
  })
  private val loadHit = loadHitVec.asUInt.orR

  private val storeAddress = storeReq.bits.addr
  private val storeSet = (storeAddress >> offsetBits)(indexBits - 1, 0)
  private val storeTag = if (tagBits == 0) 0.U else storeAddress >> (offsetBits + indexBits)
  private val storeWord = (storeAddress >> 2)(log2Ceil(lineWords) - 1, 0)
  private val storeHitVec = VecInit((0 until ways).map { way =>
    valid(storeSet)(way) && lines(storeSet)(way).tag === storeTag
  })

  private val emptyVec = VecInit((0 until ways).map(way => !valid(loadSet)(way)))
  private val victim = Mux(emptyVec.asUInt.orR, PriorityEncoder(emptyVec), 0.U)
  private val lineBase = Cat(loadAddress(31, offsetBits), 0.U(offsetBits.W))

  memReq.valid := state === fillAddrState
  memReq.bits := fillAddress
  writeReq.valid := state === write
  writeReq.bits := pendingStore

  when(loadResponseValid) {
    loadResponseValid := false.B
  }
  when(storeResponse.valid) {
    storeResponse.valid := false.B
  }

  loadResp.valid := loadResponseValid
  loadResp.bits := loadResponseData

  when(loadReq.fire) {
    when(loadHit) {
      loadResponseValid := true.B
      loadResponseData := Mux1H(loadHitVec, VecInit((0 until ways).map { way =>
        lines(loadSet)(way).data(loadWord)
      }))
    }.otherwise {
      fillAddress := lineBase
      fillTag := loadTag
      fillSet := loadSet
      fillWay := victim
      fillWord := 0.U
      requestedIndex := loadWord
      state := fillAddrState
    }
  }

  when(state === fillAddrState && memReq.ready) {
    state := fillDataState
  }

  when(state === fillDataState && memResp.valid) {
    lines(fillSet)(fillWay).data(fillWord) := memResp.bits
    when(fillWord === requestedIndex) {
      requestedData := memResp.bits
    }
    when(fillWord === (lineWords - 1).U) {
      lines(fillSet)(fillWay).tag := fillTag
      valid(fillSet)(fillWay) := true.B
      loadResponseValid := true.B
      loadResponseData := Mux(fillWord === requestedIndex, memResp.bits, requestedData)
      state := idle
    }.otherwise {
      fillAddress := fillAddress + 4.U
      fillWord := fillWord + 1.U
      state := fillAddrState
    }
  }

  when(storeReq.fire) {
    pendingStore := storeReq.bits
    when(storeHitVec.asUInt.orR) {
      for (way <- 0 until ways) {
        when(storeHitVec(way)) {
          val oldWord = lines(storeSet)(way).data(storeWord)
          val merged = Cat((0 until 4).reverse.map { byte =>
            Mux(
              storeReq.bits.strobe(byte),
              storeReq.bits.data(byte * 8 + 7, byte * 8),
              oldWord(byte * 8 + 7, byte * 8)
            )
          })
          lines(storeSet)(way).data(storeWord) := merged
        }
      }
    }
    state := write
  }

  when(state === write && writeReq.ready) {
    state := writeWait
  }

  when(state === writeWait && writeResp.valid) {
    storeResponse.valid := true.B
    storeResponse.bits := writeResp.bits
    state := idle
  }
}

class MemorySubsystem(config: CpuConfig) extends Module {
  val iReq  = IO(Flipped(Decoupled(UInt(32.W))))
  val iResp = IO(Valid(UInt(32.W)))
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

  val icache = Module(new InstructionCache(config))
  val dcache = Module(new DataCache(config))
  val reads = Module(new AxiReadArbiter)
  val writes = Module(new AxiWriteEngine)

  iReq <> icache.req
  iResp := icache.resp
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
