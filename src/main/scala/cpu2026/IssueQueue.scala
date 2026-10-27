package cpu2026

import chisel3._
import chisel3.util._

class IssueEntry(physBits: Int, robBits: Int) extends Bundle {
  val valid     = Bool()
  val seq       = UInt(32.W)
  val robIndex  = UInt(robBits.W)
  val decoded   = new Decoded
  val ps1       = UInt(physBits.W)
  val ps2       = UInt(physBits.W)
  val rs1Ready  = Bool()
  val rs2Ready  = Bool()
  val value1    = UInt(32.W)
  val value2    = UInt(32.W)
  val hasPhysDest = Bool()
  val physDest  = UInt(physBits.W)
}

class WritebackEvent(physBits: Int, robBits: Int) extends Bundle {
  val robIndex = UInt(robBits.W)
  val seq      = UInt(32.W)
  val hasPhysDest = Bool()
  val physDest = UInt(physBits.W)
  val value    = UInt(32.W)
  val branchTaken = Bool()
  val branchTarget = UInt(32.W)
  val storeData = UInt(32.W)
  val storeStrobe = UInt(4.W)
}

/** Age-ordered Tomasulo issue queue.
  *
  * Dispatch writes an instruction group into the tail. Each cycle a prefix of
  * eligible instructions is selected for execution. The vector is compacted at
  * the next edge, so index order remains a simple and reliable age order.
  */
class IssueQueue(issueWidth: Int, depth: Int, physRegs: Int, robDepth: Int) extends Module {
  private val physBits = log2Ceil(physRegs)
  private val robBits = log2Ceil(robDepth)
  private val countBits = log2Ceil(depth + 1)
  require(issueWidth >= 1 && issueWidth <= 4)
  require(depth >= issueWidth)

  val enqValid = IO(Input(Vec(issueWidth, Bool())))
  val enq      = IO(Input(Vec(issueWidth, new IssueEntry(physBits, robBits))))
  val enqReady = IO(Output(Bool()))
  val enqCommit = IO(Input(Bool()))

  val writeback = IO(Input(Vec(issueWidth + 1, Valid(new WritebackEvent(physBits, robBits)))))
  val issue     = IO(Output(Vec(issueWidth, Valid(new IssueEntry(physBits, robBits)))))
  val memoryReady = IO(Input(Bool()))
  val storePending = IO(Input(Bool()))
  val oldestStoreSeq = IO(Input(UInt(32.W)))
  val flush     = IO(Input(Valid(UInt(32.W))))
  val occupancy = IO(Output(UInt(countBits.W)))

  val entries = RegInit(
    VecInit(Seq.fill(depth)(0.U.asTypeOf(new IssueEntry(physBits, robBits))))
  )
  val count = RegInit(0.U(countBits.W))

  private val woken = Wire(Vec(depth, new IssueEntry(physBits, robBits)))
  for (i <- 0 until depth) {
    woken(i) := entries(i)
    val match1 = VecInit((0 until issueWidth + 1).map { port =>
      writeback(port).valid && writeback(port).bits.hasPhysDest &&
        writeback(port).bits.physDest === entries(i).ps1
    })
    val match2 = VecInit((0 until issueWidth + 1).map { port =>
      writeback(port).valid && writeback(port).bits.hasPhysDest &&
        writeback(port).bits.physDest === entries(i).ps2
    })
    when(entries(i).valid && !entries(i).rs1Ready && match1.asUInt.orR) {
      woken(i).rs1Ready := true.B
      woken(i).value1 := Mux1H(
        match1,
        VecInit((0 until issueWidth + 1).map(port => writeback(port).bits.value))
      )
    }
    when(entries(i).valid && !entries(i).rs2Ready && match2.asUInt.orR) {
      woken(i).rs2Ready := true.B
      woken(i).value2 := Mux1H(
        match2,
        VecInit((0 until issueWidth + 1).map(port => writeback(port).bits.value))
      )
    }
  }

  private val eligible = VecInit((0 until depth).map { i =>
    entries(i).valid && entries(i).rs1Ready && entries(i).rs2Ready
  })
  private val memoryMask = VecInit((0 until depth).map { i =>
    entries(i).valid && entries(i).decoded.isMemory
  })
  private val loadMask = VecInit((0 until depth).map { i =>
    entries(i).valid && entries(i).decoded.isLoad
  })
  private val loadBlockedByOlderStore = VecInit((0 until depth).map { i =>
    loadMask(i) && storePending && entries(i).seq > oldestStoreSeq
  }).asUInt
  private val controlMask = VecInit((0 until depth).map { i =>
    entries(i).valid && entries(i).decoded.isControl
  })

  private var selectedMask = 0.U(depth.W)
  private var memorySelected = false.B
  private var controlSelected = false.B

  for (lane <- 0 until issueWidth) {
    val memoryBlocked =
      if (lane == 0) !memoryReady else memorySelected || !memoryReady
    val afterControl = if (lane == 0) false.B else controlSelected
    val candidates = eligible.asUInt & ~selectedMask &
      Mux(memoryBlocked, ~memoryMask.asUInt, Fill(depth, true.B)) &
      ~loadBlockedByOlderStore &
      Mux(afterControl, 0.U(depth.W), Fill(depth, true.B))

    val found = candidates.orR
    val index = PriorityEncoder(candidates)
    val chosen = Mux(found, UIntToOH(index, depth), 0.U(depth.W))

    issue(lane).valid := found
    issue(lane).bits :=
      Mux(found, entries(index), 0.U.asTypeOf(new IssueEntry(physBits, robBits)))

    memorySelected = memorySelected || (found && memoryMask(index))
    controlSelected = controlSelected || (found && controlMask(index))
    selectedMask = selectedMask | chosen
  }

  private val keep = VecInit((0 until depth).map { i =>
    woken(i).valid && !selectedMask(i) &&
      !(flush.valid && woken(i).seq > flush.bits)
  })
  private val survivorIndex = VecInit((0 until depth).map { i =>
    keep.take(i).foldLeft(0.U(countBits.W))(_ +& _.asUInt)
  })
  private val survivorCount = PopCount(keep)
  private val enqueueCount = PopCount(enqValid)
  private val enqueueEnabled = enqCommit && enqReady

  private val resolvedEnqueue = Wire(
    Vec(issueWidth, new IssueEntry(physBits, robBits))
  )
  for (lane <- 0 until issueWidth) {
    resolvedEnqueue(lane) := enq(lane)
    val source1Matches = VecInit((0 until issueWidth + 1).map { port =>
      writeback(port).valid && writeback(port).bits.hasPhysDest &&
        writeback(port).bits.physDest === enq(lane).ps1
    })
    val source2Matches = VecInit((0 until issueWidth + 1).map { port =>
      writeback(port).valid && writeback(port).bits.hasPhysDest &&
        writeback(port).bits.physDest === enq(lane).ps2
    })
    when(!enq(lane).rs1Ready && source1Matches.asUInt.orR) {
      resolvedEnqueue(lane).rs1Ready := true.B
      resolvedEnqueue(lane).value1 := Mux1H(
        source1Matches,
        VecInit((0 until issueWidth + 1).map(port => writeback(port).bits.value))
      )
    }
    when(!enq(lane).rs2Ready && source2Matches.asUInt.orR) {
      resolvedEnqueue(lane).rs2Ready := true.B
      resolvedEnqueue(lane).value2 := Mux1H(
        source2Matches,
        VecInit((0 until issueWidth + 1).map(port => writeback(port).bits.value))
      )
    }
  }

  enqReady := !flush.valid && (survivorCount +& enqueueCount <= depth.U)

  private val next = Wire(Vec(depth, new IssueEntry(physBits, robBits)))
  for (i <- 0 until depth) {
    next(i) := 0.U.asTypeOf(new IssueEntry(physBits, robBits))
    when(keep(i)) {
      next(survivorIndex(i)(log2Ceil(depth) - 1, 0)) := woken(i)
    }
  }

  private var enqueueOffset = survivorCount
  for (lane <- 0 until issueWidth) {
    val append = enqueueEnabled && enqValid(lane)
    val appendIndex = enqueueOffset(log2Ceil(depth) - 1, 0)
    when(append) {
      next(appendIndex) := resolvedEnqueue(lane)
    }
    enqueueOffset = enqueueOffset + append.asUInt
  }

  entries := next
  count := survivorCount +& Mux(enqueueEnabled, enqueueCount, 0.U)
  occupancy := count
}
