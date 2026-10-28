package cpu2026

import chisel3._
import chisel3.util._

/** Two-stage unsigned 32x32 multiplier.
  *
  * Stage one splits both operands into 16-bit limbs, forms the four products of
  * the 2x2 limb convolution, aligns them to a 64-bit product, and reduces the
  * four rows with 3:2 Wallace-tree compressors. Stage two performs the single
  * wide addition of the registered sum/carry pair.
  *
  * The module accepts one operation when `start` is asserted in `Idle`.
  * `product` is a 64-bit unsigned result and `done` is asserted for one cycle
  * after the final adder has consumed the first-stage registers.
  */
class PipelinedWallaceMultiplier extends Module {
  val start   = IO(Input(Bool()))
  val a       = IO(Input(UInt(32.W)))
  val b       = IO(Input(UInt(32.W)))
  val busy    = IO(Output(Bool()))
  val done    = IO(Output(Bool()))
  val product = IO(Output(UInt(64.W)))

  private val idle :: finalAdd :: Nil = Enum(2)
  val state = RegInit(idle)

  val sumReg   = RegInit(0.U(64.W))
  val carryReg = RegInit(0.U(64.W))

  /** Compress three equally weighted rows into a sum row and one carry row.
    *
    * The majority term is the carry generated at each bit position. Shifting
    * it left once gives the carry row's weight. The final product is below
    * 2^64, so discarding the compressor's 65th bit is exact modulo the product
    * width and keeps every row in a uniform 64-bit format.
    */
  private def compress3(x: UInt, y: UInt, z: UInt): (UInt, UInt) = {
    val sumRow = Wire(UInt(64.W))
    val carryRow = Wire(UInt(64.W))
    sumRow := x ^ y ^ z
    carryRow := ((x & y) | (x & z) | (y & z)) << 1
    (sumRow, carryRow)
  }

  // A 32-bit operand is interpreted as {high limb, low limb}. Computing all
  // four limb pairs is the 2x2 convolution needed by the full product:
  //
  //   product = low*low + (low*high + high*low) << 16 + high*high << 32
  private val aLow     = a(15, 0)
  private val aHigh    = a(31, 16)
  private val bLow     = b(15, 0)
  private val bHigh    = b(31, 16)
  private val lowLow   = (aLow * bLow).pad(64)
  private val lowHigh  = ((aLow * bHigh) << 16).pad(64)
  private val highLow  = ((aHigh * bLow) << 16).pad(64)
  private val highHigh = ((aHigh * bHigh) << 32).pad(64)

  // Four rows cannot be reduced to two in one full-adder level. The first CSA
  // consumes three rows; the second consumes its two outputs plus the fourth.
  private val (firstSum, firstCarry) = compress3(lowLow, lowHigh, highLow)
  private val (wallaceSum, wallaceCarry) =
    compress3(firstSum, firstCarry, highHigh)

  busy := state =/= idle
  done := state === finalAdd
  product := sumReg + carryReg

  switch(state) {
    is(idle) {
      when(start) {
        sumReg := wallaceSum
        carryReg := wallaceCarry
        state := finalAdd
      }
    }
    is(finalAdd) {
      state := idle
    }
  }
}
