package cpu2026

import chisel3._
import chisel3.util._

/** Shared M-extension unit.
  *
  * Multiplication uses a two-stage Wallace-tree multiplier. Division/remainder
  * uses a radix-2 restoring divider and takes 34 cycles from acceptance to
  * writeback.
  */
class MulDivUnit extends Module {
  val start = IO(Input(Bool()))
  val op    = IO(Input(UInt(5.W)))
  val a     = IO(Input(UInt(32.W)))
  val b     = IO(Input(UInt(32.W)))
  val busy  = IO(Output(Bool()))
  val done  = IO(Output(Bool()))
  val value = IO(Output(UInt(32.W)))

  private val idle :: mulDone :: divInit :: divIter :: divDone :: Nil = Enum(5)
  val state = RegInit(idle)
  val opReg = Reg(UInt(5.W))
  val aReg = Reg(UInt(32.W))
  val bReg = Reg(UInt(32.W))
  val resultReg = Reg(UInt(32.W))

  val dividend = Reg(UInt(32.W))
  val divisor = Reg(UInt(32.W))
  val remainder = Reg(UInt(32.W))
  val quotient = Reg(UInt(32.W))
  val count = Reg(UInt(6.W))
  val aNegative = Reg(Bool())
  val quotientNegative = Reg(Bool())

  val isMul = op === AluOp.mul || op === AluOp.mulh ||
    op === AluOp.mulhsu || op === AluOp.mulhu
  val isSignedDiv = op === AluOp.div || op === AluOp.rem

  val multiplier = Module(new PipelinedWallaceMultiplier)
  multiplier.start := start && isMul
  multiplier.a := a
  multiplier.b := b

  // One unsigned product is sufficient for every RV32M multiply:
  // low 32 bits are sign-agnostic, and signed high halves are corrected by
  // subtracting the other operand when either source sign bit is set.
  private val unsignedProduct = multiplier.product
  private val unsignedHigh = unsignedProduct(63, 32)
  private val signedSignedHigh =
    unsignedHigh - Mux(aReg(31), bReg, 0.U) - Mux(bReg(31), aReg, 0.U)
  private val signedUnsignedHigh =
    unsignedHigh - Mux(aReg(31), bReg, 0.U)
  val multiplyValue = MuxLookup(opReg, 0.U)(
    Seq(
      AluOp.mul -> unsignedProduct(31, 0),
      AluOp.mulh -> signedSignedHigh,
      AluOp.mulhsu -> signedUnsignedHigh,
      AluOp.mulhu -> unsignedHigh
    )
  )

  val remCandidate = Cat(remainder(30, 0), dividend(31))
  val subtract = remCandidate >= divisor
  val nextRemainder = Mux(subtract, remCandidate - divisor, remCandidate)
  val nextQuotient = Cat(quotient(30, 0), subtract.asUInt)
  val unsignedQuotient = nextQuotient
  val unsignedRemainder = nextRemainder

  busy := state =/= idle || multiplier.busy
  done := state === mulDone || state === divDone
  value := Mux(state === mulDone, multiplyValue, resultReg)

  switch(state) {
    is(idle) {
      when(start) {
        opReg := op
        aReg := a
        bReg := b
        when(isMul) {
          state := mulDone
        }.otherwise {
          val signed = isSignedDiv
          val negA = signed && a(31)
          val negB = signed && b(31)
          aNegative := negA
          quotientNegative := negA ^ negB
          dividend := Mux(negA, -a, a)
          divisor := Mux(negB, -b, b)
          state := divInit
        }
      }
    }
    is(mulDone) {
      state := idle
    }
    is(divInit) {
      when(divisor === 0.U) {
        resultReg := Mux(opReg === AluOp.rem || opReg === AluOp.remu, aReg, "hffffffff".U)
        state := divDone
      }.elsewhen(aNegative && aReg === "h80000000".U && bReg === "hffffffff".U) {
        resultReg := Mux(opReg === AluOp.rem, 0.U, "h80000000".U)
        state := divDone
      }.otherwise {
        remainder := 0.U
        quotient := 0.U
        count := 0.U
        state := divIter
      }
    }
    is(divIter) {
      remainder := nextRemainder
      quotient := nextQuotient
      dividend := dividend << 1
      when(count === 31.U) {
        val signedRemainder = Mux(aNegative, -unsignedRemainder, unsignedRemainder)
        val signedQuotient = Mux(quotientNegative, -unsignedQuotient, unsignedQuotient)
        resultReg := Mux(
          opReg === AluOp.rem,
          signedRemainder,
          Mux(opReg === AluOp.remu, unsignedRemainder,
            Mux(opReg === AluOp.divu, unsignedQuotient, signedQuotient))
        )
        state := divDone
      }.otherwise {
        count := count + 1.U
      }
    }
    is(divDone) {
      state := idle
    }
  }
}
