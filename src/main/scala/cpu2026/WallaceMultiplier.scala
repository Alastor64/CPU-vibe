package cpu2026

import chisel3._
import chisel3.util._
import scala.collection.mutable.ArrayBuffer

/** Two-stage unsigned 32x32 standard Wallace-tree multiplier.
  *
  * Stage one forms the complete bit-level convolution `a[i] & b[j]`, aligns
  * every term by its weight `2^(i+j)`, and reduces all terms in each column
  * with a levelized 3:2 compressor tree. Stage two adds the two registered
  * result rows.
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

  /** A Wallace full adder consumes three bits of equal weight. */
  private def fullAdder(x: Bool, y: Bool, z: Bool): (Bool, Bool) = {
    val sum = x ^ y ^ z
    val carry = (x & y) | (x & z) | (y & z)
    (sum, carry)
  }

  /** Collapse every column to at most two bits using a levelized Wallace tree.
    *
    * All columns are compressed from the same input snapshot. Carries created
    * in column k enter only column k+1 in the next level, so no carry can ripple
    * through an unbounded chain during the tree stage.
    */
  private def compressColumns(initial: Seq[Seq[Bool]]): Seq[Seq[Bool]] = {
    var columns = initial
    var level = 0
    while (columns.exists(_.length > 2)) {
      val next = Array.fill(64)(ArrayBuffer.empty[Bool])
      for (column <- 0 until 64) {
        val bits = columns(column)
        var index = 0
        while (index + 2 < bits.length) {
          val (sum, carry) = fullAdder(bits(index), bits(index + 1), bits(index + 2))
          next(column) += sum
          if (column + 1 < 64) {
            next(column + 1) += carry
          }
          index += 3
        }
        while (index < bits.length) {
          next(column) += bits(index)
          index += 1
        }
      }
      columns = next.map(_.toSeq).toSeq
      level += 1
      require(level < 64, "Wallace tree failed to converge")
    }
    columns
  }

  private def bitAt(bits: Seq[Bool], index: Int): Bool =
    bits.lift(index).getOrElse(false.B)

  private def packedRow(columns: Seq[Seq[Bool]], index: Int): UInt =
    Cat(columns.reverse.map(column => bitAt(column, index)))

  // Each a[i]&b[j] is a one-bit convolution term at binary weight i+j.
  // Partial-product rows are implied by this column representation, avoiding
  // any embedded 16x16 or 32x32 multiplier primitive.
  private val partialProductColumns = (0 until 64).map { weight =>
    (for {
      aBit <- 0 until 32
      bBit <- 0 until 32
      if aBit + bBit == weight
    } yield a(aBit) & b(bBit)).toSeq
  }

  private val compressedColumns = compressColumns(partialProductColumns)
  private val wallaceSum = packedRow(compressedColumns, 0)
  private val wallaceCarry = packedRow(compressedColumns, 1)

  busy := state =/= idle
  done := state === finalAdd
  product := (sumReg +& carryReg)(63, 0)

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
