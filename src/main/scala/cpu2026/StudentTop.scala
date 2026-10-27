package cpu2026

import chisel3._

/** Course-visible top level.
  *
  * The ports intentionally remain flat because the evaluator expects names such
  * as `araddr`, not a Chisel bundle prefixed with `io_`.
  */
class StudentTop(config: CpuConfig = CpuConfig()) extends Module {
  override def desiredName: String = "student_top"
  override def resetType = Module.ResetType.Synchronous

  // The configuration is fixed at elaboration time. Later commits connect the
  // frontend, ROB, rename tables, issue queues, and execution units here.
  private val issueWidth: Int = config.issueWidth

  val araddr  = IO(Output(UInt(32.W)))
  val arvalid = IO(Output(Bool()))
  val arready = IO(Input(Bool()))

  val rdata   = IO(Input(UInt(32.W)))
  val rresp   = IO(Input(UInt(2.W)))
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

  // The first build establishes the interface and generation flow. No bus
  // transaction is issued until the real fetch/memory pipeline is connected.
  araddr  := 0.U
  arvalid := false.B
  rready  := false.B
  awaddr  := 0.U
  awvalid := false.B
  wdata   := 0.U
  wstrb   := 0.U
  wvalid  := false.B
  bready  := false.B
}
