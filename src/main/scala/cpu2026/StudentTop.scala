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

  val core = Module(new Core(config))
  val memory = Module(new MemorySubsystem(config))

  core.iReq <> memory.iReq
  core.iResp := memory.iResp
  core.dLoadReq <> memory.dLoadReq
  core.dLoadResp := memory.dLoadResp
  core.dStoreReq <> memory.dStoreReq
  core.dStoreResp := memory.dStoreResp
  dontTouch(core.halted)

  araddr := memory.araddr
  arvalid := memory.arvalid
  memory.arready := arready
  memory.rdata := rdata
  memory.rvalid := rvalid
  rready := memory.rready

  awaddr := memory.awaddr
  awvalid := memory.awvalid
  memory.awready := awready
  wdata := memory.wdata
  wstrb := memory.wstrb
  wvalid := memory.wvalid
  memory.wready := wready
  memory.bresp := bresp
  memory.bvalid := bvalid
  bready := memory.bready
}
