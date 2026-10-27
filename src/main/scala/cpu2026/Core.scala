package cpu2026

import chisel3._
import chisel3.util._

class FetchEntry extends Bundle {
  val pc    = UInt(32.W)
  val instr = UInt(32.W)
}

class RobEntry(physBits: Int) extends Bundle {
  val valid = Bool()
  val done  = Bool()
  val seq   = UInt(32.W)
  val decoded = new Decoded
  val hasPhysDest = Bool()
  val archDest = UInt(5.W)
  val newPhys  = UInt(physBits.W)
  val oldPhys  = UInt(physBits.W)
  val snapshot = Vec(32, UInt(physBits.W))
  val result   = UInt(32.W)
  val branchTaken = Bool()
  val branchTarget = UInt(32.W)
  val storeData = UInt(32.W)
  val storeStrobe = UInt(4.W)
}

/** Tomasulo core with speculative physical-register renaming and in-order ROB
  * commit. Branches are predicted not-taken; taken controls redirect from the
  * execute stage and restore the branch's rename snapshot.
  */
class Core(config: CpuConfig) extends Module {
  private val debugTrace = sys.env.get("CPU_DEBUG").contains("1")
  private val width = config.issueWidth
  private val physBits = log2Ceil(config.physicalRegisters)
  private val robBits = log2Ceil(config.robEntries)
  private val robCountBits = log2Ceil(config.robEntries + 1)
  private val fetchDepth = math.max(16, width * 4)
  private val fetchCountBits = log2Ceil(fetchDepth + 1)
  private val dispatchCountBits = log2Ceil(width + 1)
  require(config.physicalRegisters <= (1 << physBits))
  require(config.robEntries <= (1 << robBits))

  val iReq = IO(Decoupled(UInt(32.W)))
  val iResp = IO(Input(Valid(UInt(32.W))))
  val dLoadReq = IO(Decoupled(UInt(32.W)))
  val dLoadResp = IO(Input(Valid(UInt(32.W))))
  val dStoreReq = IO(Decoupled(new StoreRequest))
  val dStoreResp = IO(Input(Valid(UInt(2.W))))

  val halted = IO(Output(Bool()))

  // -----------------------------------------------------------------------
  // Front end
  // -----------------------------------------------------------------------
  val fetchPc = RegInit(0.U(32.W))
  val fetchInFlight = RegInit(false.B)
  val dropFetchResponse = RegInit(false.B)
  val fetchQueue = Reg(Vec(fetchDepth, new FetchEntry))
  val fetchQueueCount = RegInit(0.U(fetchCountBits.W))

  val decoders = Seq.fill(width)(Module(new Decoder))
  val decoded = Wire(Vec(width, new Decoded))
  for (lane <- 0 until width) {
    val validSlot = lane.U < fetchQueueCount
    decoders(lane).pc := Mux(validSlot, fetchQueue(lane).pc, 0.U)
    decoders(lane).in := Mux(validSlot, fetchQueue(lane).instr, 0.U)
    decoded(lane) := decoders(lane).out
  }

  val prefetchCount = Mux(fetchQueueCount >= width.U, width.U, fetchQueueCount)
  val controlSlots = VecInit((0 until width).map { lane =>
    lane.U < prefetchCount && decoded(lane).isControl
  })
  val candidateCount = Mux(
    controlSlots.asUInt.orR,
    PriorityEncoder(controlSlots) + 1.U,
    prefetchCount
  )
  val slotValid = VecInit((0 until width).map(lane => lane.U < candidateCount))

  // -----------------------------------------------------------------------
  // Physical register file, renaming maps, and allocation bit map
  // -----------------------------------------------------------------------
  val physicalRegisters = RegInit(
    VecInit(Seq.fill(config.physicalRegisters)(0.U(32.W)))
  )
  val physicalReady = RegInit(
    VecInit(Seq.fill(config.physicalRegisters)(true.B))
  )
  val speculativeMap = RegInit(
    VecInit((0 until 32).map(index => index.U(physBits.W)))
  )
  val commitMap = RegInit(
    VecInit((0 until 32).map(index => index.U(physBits.W)))
  )
  val allocated = RegInit(
    VecInit((0 until config.physicalRegisters).map(index => (index < 32).B))
  )

  val freePhysical = VecInit(allocated.map(!_))
  val needsPhysical = VecInit((0 until width).map { lane =>
    slotValid(lane) && decoded(lane).writesRd
  })
  val physicalNeedCount = PopCount(needsPhysical)

  private var allocationUsed = 0.U(config.physicalRegisters.W)
  val newPhysical = Wire(Vec(width, UInt(physBits.W)))
  for (lane <- 0 until width) {
    val available = freePhysical.asUInt & ~allocationUsed
    val phys = PriorityEncoder(available)
    newPhysical(lane) := Mux(needsPhysical(lane), phys, 0.U)
    when(needsPhysical(lane)) {
      allocationUsed = allocationUsed | UIntToOH(phys, config.physicalRegisters)
    }
  }
  val enoughPhysical = PopCount(freePhysical) >= physicalNeedCount

  // Mapping at the beginning of each dispatch lane, including earlier writes
  // in the same dispatch group. This also provides cross-lane wakeup by tag.
  def mappingBefore(lane: Int): Vec[UInt] =
    VecInit((0 until 32).map { arch =>
      var mapping = speculativeMap(arch)
      for (previous <- 0 until lane) {
        mapping = Mux(
          slotValid(previous) && decoded(previous).writesRd &&
            decoded(previous).rd === arch.U,
          newPhysical(previous),
          mapping
        )
      }
      mapping
    })

  def mappingAfter(lane: Int): Vec[UInt] =
    VecInit((0 until 32).map { arch =>
      var mapping = speculativeMap(arch)
      for (previous <- 0 to lane) {
        mapping = Mux(
          slotValid(previous) && decoded(previous).writesRd &&
            decoded(previous).rd === arch.U,
          newPhysical(previous),
          mapping
        )
      }
      mapping
    })

  // -----------------------------------------------------------------------
  // Reorder buffer and issue queue
  // -----------------------------------------------------------------------
  val rob = Reg(Vec(config.robEntries, new RobEntry(physBits)))
  val robHead = RegInit(0.U(robBits.W))
  val robTail = RegInit(0.U(robBits.W))
  val robCount = RegInit(0.U(robCountBits.W))
  val nextSequence = RegInit(0.U(32.W))

  val issueQueue = Module(
    new IssueQueue(width, config.issueQueueEntries, config.physicalRegisters, config.robEntries)
  )
  issueQueue.enqValid := slotValid

  for (lane <- 0 until width) {
    val before = mappingBefore(lane)
    val rs1 = decoded(lane).rs1
    val rs2 = decoded(lane).rs2

    val rs1Earlier =
      if (lane == 0) VecInit(Seq(false.B))
      else VecInit((0 until lane).map { previous =>
        slotValid(previous) && decoded(previous).writesRd &&
          decoded(previous).rd === rs1
      })
    val rs2Earlier =
      if (lane == 0) VecInit(Seq(false.B))
      else VecInit((0 until lane).map { previous =>
        slotValid(previous) && decoded(previous).writesRd &&
          decoded(previous).rd === rs2
      })
    val rs1EarlierPhys =
      if (lane == 0) 0.U
      else Mux1H(rs1Earlier, VecInit((0 until lane).map(newPhysical)))
    val rs2EarlierPhys =
      if (lane == 0) 0.U
      else Mux1H(rs2Earlier, VecInit((0 until lane).map(newPhysical)))
    val ps1 = Mux(rs1Earlier.asUInt.orR, rs1EarlierPhys, before(rs1))
    val ps2 = Mux(rs2Earlier.asUInt.orR, rs2EarlierPhys, before(rs2))
    val ready1 = Mux(rs1Earlier.asUInt.orR, false.B, ps1 === 0.U || physicalReady(ps1))
    val ready2 = Mux(rs2Earlier.asUInt.orR, false.B, ps2 === 0.U || physicalReady(ps2))

    issueQueue.enq(lane).valid := true.B
    issueQueue.enq(lane).seq := nextSequence + lane.U
    issueQueue.enq(lane).robIndex := (robTail + lane.U)(robBits - 1, 0)
    issueQueue.enq(lane).decoded := decoded(lane)
    issueQueue.enq(lane).ps1 := ps1
    issueQueue.enq(lane).ps2 := ps2
    issueQueue.enq(lane).rs1Ready := ready1
    issueQueue.enq(lane).rs2Ready := ready2
    issueQueue.enq(lane).value1 :=
      Mux(rs1Earlier.asUInt.orR, 0.U, Mux(ps1 === 0.U, 0.U, physicalRegisters(ps1)))
    issueQueue.enq(lane).value2 :=
      Mux(rs2Earlier.asUInt.orR, 0.U, Mux(ps2 === 0.U, 0.U, physicalRegisters(ps2)))
    issueQueue.enq(lane).hasPhysDest := needsPhysical(lane)
    issueQueue.enq(lane).physDest := newPhysical(lane)
  }

  val robHasRoom = robCount +& candidateCount <= config.robEntries.U

  // -----------------------------------------------------------------------
  // Execution and common data bus
  // -----------------------------------------------------------------------
  val alus = Seq.fill(width)(Module(new Alu))
  val loadInFlight = RegInit(false.B)
  val loadRobIndex = Reg(UInt(robBits.W))
  val loadSequence = Reg(UInt(32.W))
  val loadPhysDest = Reg(UInt(physBits.W))
  val loadHasPhysDest = Reg(Bool())
  val loadAddress = Reg(UInt(32.W))
  val loadFunct3 = Reg(UInt(3.W))
  val loadDiscard = RegInit(false.B)

  val memoryReady = !loadInFlight && dLoadReq.ready
  var pendingStoreFound = false.B
  var oldestPendingStoreSeq = "hffffffff".U
  for (index <- 0 until config.robEntries) {
    val age = (index.U - robHead)(robBits - 1, 0)
    val pending = rob(index).valid && age < robCount && rob(index).decoded.isStore
    oldestPendingStoreSeq = Mux(
      pending && (!pendingStoreFound || rob(index).seq < oldestPendingStoreSeq),
      rob(index).seq,
      oldestPendingStoreSeq
    )
    pendingStoreFound = pendingStoreFound || pending
  }
  issueQueue.memoryReady := memoryReady
  issueQueue.storePending := pendingStoreFound
  issueQueue.oldestStoreSeq := oldestPendingStoreSeq
  issueQueue.enqCommit := false.B // replaced after redirect and capacity are known

  val writeback = Wire(
    Vec(width + 1, Valid(new WritebackEvent(physBits, robBits)))
  )
  for (port <- 0 until width + 1) {
    writeback(port) := 0.U.asTypeOf(Valid(new WritebackEvent(physBits, robBits)))
  }

  val redirectLanes = VecInit((0 until width).map { lane =>
    issueQueue.issue(lane).valid && alus(lane).redirect
  })
  val redirectNow = redirectLanes.asUInt.orR
  val redirectLane = PriorityEncoder(redirectLanes)
  val redirectSequence = issueQueue.issue(redirectLane).bits.seq
  val redirectTarget = VecInit(alus.map(_.target))(redirectLane)
  val redirectRobIndex = issueQueue.issue(redirectLane).bits.robIndex

  for (lane <- 0 until width) {
    val entry = issueQueue.issue(lane).bits
    val dec = entry.decoded
    val operandA = MuxLookup(dec.op1Sel, entry.value1)(
      Seq(
        Op1Sel.rs1 -> entry.value1,
        Op1Sel.pc -> dec.pc,
        Op1Sel.zero -> 0.U
      )
    )
    val operandB = MuxLookup(dec.op2Sel, entry.value2)(
      Seq(
        Op2Sel.rs2 -> entry.value2,
        Op2Sel.imm -> dec.imm,
        Op2Sel.four -> 4.U
      )
    )
    alus(lane).valid := issueQueue.issue(lane).valid
    alus(lane).op := dec.aluOp
    alus(lane).decoded := dec
    alus(lane).a := operandA
    alus(lane).b := operandB

    val allowed = !redirectNow || entry.seq <= redirectSequence
    val result = alus(lane).result
    val byteOffset = result(1, 0)
    val byteStrobe = UIntToOH(byteOffset, 4)
    val halfStrobe = Mux(result(1), "hc".U(4.W), "h3".U(4.W))
    val storeStrobe = MuxLookup(dec.memFunct3, "hf".U(4.W))(
      Seq(
        "b000".U -> byteStrobe,
        "b001".U -> halfStrobe,
        "b010".U -> "hf".U
      )
    )
    val storeData = MuxLookup(dec.memFunct3, entry.value2)(
      Seq(
        "b000".U -> (entry.value2 << (byteOffset << 3))(31, 0),
        "b001".U -> Mux(result(1), entry.value2 << 16, entry.value2),
        "b010".U -> entry.value2
      )
    )
    val writebackValue =
      Mux(dec.isJal || dec.isJalr, alus(lane).link, result)

    writeback(lane).valid := issueQueue.issue(lane).valid && allowed && !dec.isLoad
    writeback(lane).bits.robIndex := entry.robIndex
    writeback(lane).bits.seq := entry.seq
    writeback(lane).bits.hasPhysDest := entry.hasPhysDest
    writeback(lane).bits.physDest := entry.physDest
    writeback(lane).bits.value := writebackValue
    writeback(lane).bits.branchTaken := alus(lane).branchTaken
    writeback(lane).bits.branchTarget := alus(lane).target
    writeback(lane).bits.storeData := storeData
    writeback(lane).bits.storeStrobe := storeStrobe
  }

  val selectedLoad = VecInit((0 until width).map { lane =>
    issueQueue.issue(lane).valid && issueQueue.issue(lane).bits.decoded.isLoad
  })
  dLoadReq.valid := selectedLoad.asUInt.orR
  dLoadReq.bits := Mux1H(
    selectedLoad,
    VecInit((0 until width).map(lane => alus(lane).result & "hfffffffc".U))
  )

  val loadPort = width
  val loadOffset = loadAddress(1, 0)
  val loadWord = dLoadResp.bits
  val loadByte = (loadWord >> (loadOffset << 3))(7, 0)
  val loadHalf = Mux(
    loadAddress(1),
    (loadWord >> 16)(15, 0),
    loadWord(15, 0)
  )
  val loadData = MuxLookup(loadFunct3, loadWord)(
    Seq(
      "b000".U -> loadByte.asSInt.pad(32).asUInt,
      "b001".U -> loadHalf.asSInt.pad(32).asUInt,
      "b010".U -> loadWord,
      "b100".U -> loadByte,
      "b101".U -> loadHalf
    )
  )
  writeback(loadPort).valid := dLoadResp.valid && !loadDiscard &&
    (!redirectNow || loadSequence < redirectSequence)
  writeback(loadPort).bits.robIndex := loadRobIndex
  writeback(loadPort).bits.seq := loadSequence
  writeback(loadPort).bits.hasPhysDest := loadHasPhysDest
  writeback(loadPort).bits.physDest := loadPhysDest
  writeback(loadPort).bits.value := loadData
  writeback(loadPort).bits.branchTaken := false.B
  writeback(loadPort).bits.branchTarget := 0.U
  writeback(loadPort).bits.storeData := 0.U
  writeback(loadPort).bits.storeStrobe := 0.U
  issueQueue.writeback := writeback

  when(dLoadReq.fire) {
    val selectedIndex = PriorityEncoder(selectedLoad)
    loadInFlight := true.B
    loadRobIndex := issueQueue.issue(selectedIndex).bits.robIndex
    loadSequence := issueQueue.issue(selectedIndex).bits.seq
    loadPhysDest := issueQueue.issue(selectedIndex).bits.physDest
    loadHasPhysDest := issueQueue.issue(selectedIndex).bits.hasPhysDest
    loadAddress := VecInit(alus.map(_.result))(selectedIndex)
    loadFunct3 := issueQueue.issue(selectedIndex).bits.decoded.memFunct3
  }
  when(dLoadResp.valid) {
    loadInFlight := false.B
    loadDiscard := false.B
  }

  // -----------------------------------------------------------------------
  // Commit and redirect
  // -----------------------------------------------------------------------
  val headEntry = rob(robHead)
  val headReady = robCount =/= 0.U && headEntry.valid && headEntry.done
  val headIsStore = headReady && headEntry.decoded.isStore
  val storeCommitActive = RegInit(false.B)
  val storeResponsePending = RegInit(false.B)
  val storeExit = RegInit(false.B)
  val haltedReg = RegInit(false.B)
  val debugCycle = RegInit(0.U(10.W))
  debugCycle := debugCycle + 1.U

  dStoreReq.valid := !redirectNow && !haltedReg && !storeCommitActive &&
    !storeResponsePending && headIsStore
  dStoreReq.bits.addr := headEntry.result & "hfffffffc".U
  dStoreReq.bits.data := headEntry.storeData
  dStoreReq.bits.strobe := headEntry.storeStrobe

  val exitStore = headEntry.result === "h80000000".U &&
    headEntry.storeStrobe === "hf".U
  when(dStoreReq.fire) {
    storeCommitActive := true.B
    storeExit := exitStore
    if (debugTrace) {
      printf(
        p"STORE_TO_BUS pc=${headEntry.decoded.pc} addr=${dStoreReq.bits.addr} " +
          p"data=${dStoreReq.bits.data} strobe=${dStoreReq.bits.strobe}\n"
      )
    }
  }

  val storeResponseCommit = !redirectNow &&
    ((dStoreResp.valid && storeCommitActive) || storeResponsePending)
  val commitEligible = VecInit((0 until width).map { offset =>
    offset.U < robCount &&
      rob((robHead + offset.U)(robBits - 1, 0)).valid &&
      rob((robHead + offset.U)(robBits - 1, 0)).done &&
      !rob((robHead + offset.U)(robBits - 1, 0)).decoded.isStore
  })
  val prefixCommit = VecInit((0 until width).map { offset =>
    (0 to offset).map(index => commitEligible(index)).reduce(_ && _)
  })
  val nonStoreCommitCount = PopCount(prefixCommit)
  val nonStoreCommit = !headIsStore && !storeCommitActive && !redirectNow &&
    !storeResponseCommit && nonStoreCommitCount =/= 0.U
  val commitCount =
    Mux(storeResponseCommit, 1.U, Mux(nonStoreCommit, nonStoreCommitCount, 0.U))

  for (offset <- 0 until width) {
    when(commitCount > offset.U) {
      val entry = rob((robHead + offset.U)(robBits - 1, 0))
      rob((robHead + offset.U)(robBits - 1, 0)).valid := false.B
      when(entry.hasPhysDest) {
        commitMap(entry.archDest) := entry.newPhys
        when(entry.oldPhys =/= 0.U) {
          allocated(entry.oldPhys) := false.B
        }
      }
      if (debugTrace) {
        printf(
          p"COMMIT pc=${entry.decoded.pc} instr=${entry.decoded.instr} " +
            p"result=${entry.result} arch=${entry.archDest} phys=${entry.newPhys} " +
            p"store=${entry.storeData}\n"
        )
      }
    }
  }
  for (port <- 0 until width + 1) {
    when(writeback(port).valid) {
      val index = writeback(port).bits.robIndex
      when(rob(index).valid && rob(index).seq === writeback(port).bits.seq) {
        rob(index).done := true.B
        rob(index).result := writeback(port).bits.value
        rob(index).branchTaken := writeback(port).bits.branchTaken
        rob(index).branchTarget := writeback(port).bits.branchTarget
        rob(index).storeData := writeback(port).bits.storeData
        rob(index).storeStrobe := writeback(port).bits.storeStrobe
      }
    }
  }

  // PRF writeback.
  for (port <- 0 until width + 1) {
    when(writeback(port).valid && writeback(port).bits.hasPhysDest) {
      physicalRegisters(writeback(port).bits.physDest) := writeback(port).bits.value
      physicalReady(writeback(port).bits.physDest) := true.B
    }
  }

  // Dispatch writes ROB entries and speculative mappings only when the whole
  // group fits. A later branch in the group truncates candidateCount.
  val dispatchAllowed = candidateCount =/= 0.U && robHasRoom && enoughPhysical &&
    issueQueue.enqReady && !redirectNow && !haltedReg
  issueQueue.enqCommit := dispatchAllowed

  when(dispatchAllowed) {
    for (lane <- 0 until width) {
      when(slotValid(lane)) {
        val index = (robTail + lane.U)(robBits - 1, 0)
        rob(index).valid := true.B
        rob(index).done := false.B
        rob(index).seq := nextSequence + lane.U
        rob(index).decoded := decoded(lane)
        rob(index).hasPhysDest := decoded(lane).writesRd
        rob(index).archDest := decoded(lane).rd
        rob(index).newPhys := newPhysical(lane)
        rob(index).oldPhys := mappingBefore(lane)(decoded(lane).rd)
        rob(index).snapshot := mappingAfter(lane)
        rob(index).result := 0.U
        rob(index).branchTaken := false.B
        rob(index).branchTarget := 0.U
        rob(index).storeData := 0.U
        rob(index).storeStrobe := 0.U
        when(decoded(lane).writesRd) {
          speculativeMap(decoded(lane).rd) := newPhysical(lane)
          allocated(newPhysical(lane)) := true.B
          physicalReady(newPhysical(lane)) := false.B
        }
      }
    }
    nextSequence := nextSequence + candidateCount
    robTail := (robTail + candidateCount)(robBits - 1, 0)
  }

  val flushedRobCount = PopCount(VecInit((0 until config.robEntries).map { index =>
    rob(index).valid && rob(index).seq > redirectSequence
  }))
  when(redirectNow) {
    issueQueue.flush.valid := true.B
    issueQueue.flush.bits := redirectSequence
    for (index <- 0 until config.robEntries) {
      when(rob(index).valid && rob(index).seq > redirectSequence) {
        rob(index).valid := false.B
        when(rob(index).hasPhysDest && rob(index).newPhys =/= 0.U) {
          allocated(rob(index).newPhys) := false.B
        }
      }
    }
    speculativeMap := rob(redirectRobIndex).snapshot
    val retainedCount = robCount - flushedRobCount
    robCount := retainedCount
    robTail := (robHead + retainedCount)(robBits - 1, 0)
    when(loadInFlight && !dLoadResp.valid && loadSequence > redirectSequence) {
      loadDiscard := true.B
    }
    if (debugTrace) {
      printf(
        p"REDIRECT from pc=${rob(redirectRobIndex).decoded.pc} " +
          p"target=${redirectTarget} seq=${redirectSequence}\n"
      )
    }
  }.otherwise {
    issueQueue.flush.valid := false.B
    issueQueue.flush.bits := 0.U
    when(commitCount =/= 0.U) {
      robHead := (robHead + commitCount)(robBits - 1, 0)
    }
    robCount := robCount - commitCount +
      Mux(dispatchAllowed, candidateCount, 0.U)
  }

  when(dStoreResp.valid && storeCommitActive) {
    storeCommitActive := false.B
    when(redirectNow) {
      storeResponsePending := true.B
    }.otherwise {
      when(storeExit) { haltedReg := true.B }
    }
  }
  when(storeResponsePending && storeResponseCommit) {
    storeResponsePending := false.B
    when(storeExit) { haltedReg := true.B }
  }
  if (debugTrace) {
    when(debugCycle === 0.U) {
      printf(
        p"STATE rob=${robCount} headPc=${headEntry.decoded.pc} " +
          p"valid=${headEntry.valid} done=${headEntry.done} " +
          p"store=${headEntry.decoded.isStore} active=${storeCommitActive} " +
          p"loadFlight=${loadInFlight} storePending=${pendingStoreFound} " +
          p"free=${PopCount(freePhysical)} iq=${issueQueue.occupancy} " +
          p"fetch=${fetchQueueCount}\n"
      )
    }
  }
  halted := haltedReg

  // -----------------------------------------------------------------------
  // Fetch queue movement and PC redirection
  // -----------------------------------------------------------------------
  val dequeueCount = Mux(dispatchAllowed, candidateCount, 0.U)
  val fetchResponseAccept = iResp.valid && !dropFetchResponse && !redirectNow
  val retainedFetch = Mux(redirectNow, 0.U, fetchQueueCount - dequeueCount)
  for (index <- 0 until fetchDepth) {
    when(!redirectNow && index.U >= dequeueCount && index.U < fetchQueueCount) {
      fetchQueue((index.U - dequeueCount)(log2Ceil(fetchDepth) - 1, 0)) :=
        fetchQueue(index)
    }
  }
  when(fetchResponseAccept) {
    fetchQueue(retainedFetch(log2Ceil(fetchDepth) - 1, 0)) := {
      val entry = Wire(new FetchEntry)
      entry.pc := fetchPc
      entry.instr := iResp.bits
      entry
    }
    if (debugTrace) {
      printf(p"FETCH pc=${fetchPc} instr=${iResp.bits}\n")
    }
  }
  fetchQueueCount := retainedFetch + Mux(fetchResponseAccept, 1.U, 0.U)

  iReq.valid := !fetchInFlight && !haltedReg && fetchQueueCount < fetchDepth.U &&
    !dropFetchResponse && !redirectNow
  iReq.bits := fetchPc
  when(iReq.fire) {
    fetchInFlight := true.B
  }
  when(iResp.valid) {
    fetchInFlight := false.B
    dropFetchResponse := false.B
  }

  when(redirectNow) {
    fetchPc := redirectTarget
    dropFetchResponse := fetchInFlight && !iResp.valid
    fetchInFlight := Mux(iResp.valid, false.B, fetchInFlight)
  }.elsewhen(fetchResponseAccept) {
    fetchPc := fetchPc + 4.U
  }
}
