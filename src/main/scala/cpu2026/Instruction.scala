package cpu2026

import chisel3._
import chisel3.util._

object FuType {
  val alu = 0.U(1.W)
  val mem = 1.U(1.W)
}

object AluOp {
  val add     = 0.U(5.W)
  val sub     = 1.U(5.W)
  val sll     = 2.U(5.W)
  val slt     = 3.U(5.W)
  val sltu    = 4.U(5.W)
  val xor     = 5.U(5.W)
  val srl     = 6.U(5.W)
  val sra     = 7.U(5.W)
  val or      = 8.U(5.W)
  val and     = 9.U(5.W)
  val mul     = 10.U(5.W)
  val mulh    = 11.U(5.W)
  val mulhsu  = 12.U(5.W)
  val mulhu   = 13.U(5.W)
  val div     = 14.U(5.W)
  val divu    = 15.U(5.W)
  val rem     = 16.U(5.W)
  val remu    = 17.U(5.W)
}

object BranchOp {
  val equal     = 0.U(3.W)
  val notEqual  = 1.U(3.W)
  val less      = 2.U(3.W)
  val greaterEq = 3.U(3.W)
  val lessU     = 4.U(3.W)
  val greaterEqU = 5.U(3.W)
}

object Op1Sel {
  val rs1  = 0.U(2.W)
  val pc   = 1.U(2.W)
  val zero = 2.U(2.W)
}

object Op2Sel {
  val rs2 = 0.U(2.W)
  val imm = 1.U(2.W)
  val four = 2.U(2.W)
}

/** Decoded fields retained by the issue queue and ROB.
  *
  * Memory operations keep the raw funct3 so sign extension and byte enables are
  * selected only after the address and write data are known.
  */
class Decoded extends Bundle {
  val pc       = UInt(32.W)
  val instr    = UInt(32.W)
  val rs1      = UInt(5.W)
  val rs2      = UInt(5.W)
  val rd       = UInt(5.W)
  val imm      = UInt(32.W)
  val fu       = UInt(1.W)
  val aluOp    = UInt(5.W)
  val op1Sel   = UInt(2.W)
  val op2Sel   = UInt(2.W)
  val isBranch = Bool()
  val branchOp = UInt(3.W)
  val isJal    = Bool()
  val isJalr   = Bool()
  val predictTaken = Bool()
  val predictTarget = UInt(32.W)
  val isLoad   = Bool()
  val isStore  = Bool()
  val memFunct3 = UInt(3.W)
  val illegal  = Bool()

  def writesRd: Bool = rd =/= 0.U
  def isControl: Bool = isBranch || isJal || isJalr
  def isMemory: Bool = isLoad || isStore
}

object Decoded {
  def default(pc: UInt, instr: UInt): Decoded = {
    val decoded = Wire(new Decoded)
    decoded.pc := pc
    decoded.instr := instr
    decoded.rs1 := 0.U
    decoded.rs2 := 0.U
    decoded.rd := 0.U
    decoded.imm := 0.U
    decoded.fu := FuType.alu
    decoded.aluOp := AluOp.add
    decoded.op1Sel := Op1Sel.rs1
    decoded.op2Sel := Op2Sel.rs2
    decoded.isBranch := false.B
    decoded.branchOp := BranchOp.equal
    decoded.isJal := false.B
    decoded.isJalr := false.B
    decoded.predictTaken := false.B
    decoded.predictTarget := 0.U
    decoded.isLoad := false.B
    decoded.isStore := false.B
    decoded.memFunct3 := 0.U
    decoded.illegal := false.B
    decoded
  }
}

object ImmGen {
  def i(instr: UInt): UInt = instr(31, 20).asSInt.pad(32).asUInt
  def s(instr: UInt): UInt =
    Cat(instr(31, 25), instr(11, 7)).asSInt.pad(32).asUInt
  def b(instr: UInt): UInt =
    Cat(instr(31), instr(7), instr(30, 25), instr(11, 8), 0.U(1.W))
      .asSInt
      .pad(32)
      .asUInt
  def u(instr: UInt): UInt = Cat(instr(31, 12), 0.U(12.W))
  def j(instr: UInt): UInt =
    Cat(instr(31), instr(19, 12), instr(20), instr(30, 21), 0.U(1.W))
      .asSInt
      .pad(32)
      .asUInt
}

class Decoder extends Module {
  val pc  = IO(Input(UInt(32.W)))
  val in  = IO(Input(UInt(32.W)))
  val out = IO(Output(new Decoded))

  private val opcode = in(6, 0)
  private val funct3 = in(14, 12)
  private val funct7 = in(31, 25)
  private def oneOf(value: UInt, choices: UInt*): Bool =
    choices.map(value === _).reduce(_ || _)

  val decoded = WireInit(Decoded.default(pc, in))

  decoded.instr := in
  decoded.rs1 := in(19, 15)
  decoded.rs2 := in(24, 20)
  decoded.rd := in(11, 7)

  // Default any instruction outside the supported base to illegal. Individual
  // matches below override this bit only after their encoding is validated.
  decoded.illegal := true.B

  switch(opcode) {
    is("h37".U) { // LUI
      decoded.illegal := false.B
      decoded.imm := ImmGen.u(in)
      decoded.op1Sel := Op1Sel.zero
      decoded.op2Sel := Op2Sel.imm
    }
    is("h17".U) { // AUIPC
      decoded.illegal := false.B
      decoded.imm := ImmGen.u(in)
      decoded.op1Sel := Op1Sel.pc
      decoded.op2Sel := Op2Sel.imm
    }
    is("h6f".U) { // JAL
      decoded.illegal := false.B
      decoded.imm := ImmGen.j(in)
      decoded.isJal := true.B
      decoded.predictTaken := true.B
      decoded.predictTarget := pc + ImmGen.j(in)
      decoded.op1Sel := Op1Sel.pc
      decoded.op2Sel := Op2Sel.four
    }
    is("h67".U) { // JALR
      when(funct3 === 0.U) {
        decoded.illegal := false.B
        decoded.imm := ImmGen.i(in)
        decoded.isJalr := true.B
        decoded.op1Sel := Op1Sel.rs1
        decoded.op2Sel := Op2Sel.four
      }
    }
    is("h63".U) { // Conditional branches
      decoded.illegal := false.B
      decoded.rd := 0.U
      decoded.imm := ImmGen.b(in)
      decoded.isBranch := true.B
      decoded.predictTaken := ImmGen.b(in).asSInt < 0.S
      decoded.predictTarget := pc + ImmGen.b(in)
      decoded.op1Sel := Op1Sel.rs1
      decoded.op2Sel := Op2Sel.rs2
      decoded.branchOp := MuxLookup(funct3, BranchOp.equal)(
        Seq(
          "b000".U -> BranchOp.equal,
          "b001".U -> BranchOp.notEqual,
          "b100".U -> BranchOp.less,
          "b101".U -> BranchOp.greaterEq,
          "b110".U -> BranchOp.lessU,
          "b111".U -> BranchOp.greaterEqU
        )
      )
      when(!oneOf(funct3, "b000".U, "b001".U, "b100".U, "b101".U, "b110".U, "b111".U)) {
        decoded.illegal := true.B
      }
    }
    is("h03".U) { // Loads
      decoded.illegal := false.B
      decoded.imm := ImmGen.i(in)
      decoded.fu := FuType.mem
      decoded.isLoad := true.B
      decoded.memFunct3 := funct3
      decoded.op1Sel := Op1Sel.rs1
      decoded.op2Sel := Op2Sel.imm
      when(!oneOf(funct3, "b000".U, "b001".U, "b010".U, "b100".U, "b101".U)) {
        decoded.illegal := true.B
      }
    }
    is("h23".U) { // Stores
      decoded.illegal := false.B
      decoded.rd := 0.U
      decoded.imm := ImmGen.s(in)
      decoded.fu := FuType.mem
      decoded.isStore := true.B
      decoded.memFunct3 := funct3
      decoded.op1Sel := Op1Sel.rs1
      decoded.op2Sel := Op2Sel.imm
      when(!oneOf(funct3, "b000".U, "b001".U, "b010".U)) {
        decoded.illegal := true.B
      }
    }
    is("h13".U) { // OP-IMM
      decoded.illegal := false.B
      decoded.imm := ImmGen.i(in)
      decoded.op2Sel := Op2Sel.imm
      decoded.aluOp := MuxLookup(funct3, AluOp.add)(
        Seq(
          "b000".U -> AluOp.add,
          "b010".U -> AluOp.slt,
          "b011".U -> AluOp.sltu,
          "b100".U -> AluOp.xor,
          "b110".U -> AluOp.or,
          "b111".U -> AluOp.and,
          "b001".U -> AluOp.sll
        )
      )
      when(funct3 === "b101".U) {
        decoded.aluOp := Mux(funct7 === "h20".U, AluOp.sra, AluOp.srl)
        when(funct7 =/= "h00".U && funct7 =/= "h20".U) {
          decoded.illegal := true.B
        }
      }.elsewhen(funct3 === "b001".U && funct7 =/= "h00".U) {
        decoded.illegal := true.B
      }.elsewhen(
        !oneOf(
          funct3,
          "b000".U,
          "b001".U,
          "b010".U,
          "b011".U,
          "b100".U,
          "b101".U,
          "b110".U,
          "b111".U
        )
      ) {
        decoded.illegal := true.B
      }
    }
    is("h33".U) { // OP
      decoded.illegal := false.B
      when(funct7 === "h01".U) {
        decoded.aluOp := MuxLookup(funct3, AluOp.mul)(
          Seq(
            "b000".U -> AluOp.mul,
            "b001".U -> AluOp.mulh,
            "b010".U -> AluOp.mulhsu,
            "b011".U -> AluOp.mulhu,
            "b100".U -> AluOp.div,
            "b101".U -> AluOp.divu,
            "b110".U -> AluOp.rem,
            "b111".U -> AluOp.remu
          )
        )
      }.otherwise {
        decoded.aluOp := MuxLookup(Cat(funct7(5), funct3), AluOp.add)(
          Seq(
            "b0000".U -> AluOp.add,
            "b1000".U -> AluOp.sub,
            "b0001".U -> AluOp.sll,
            "b0010".U -> AluOp.slt,
            "b0011".U -> AluOp.sltu,
            "b0100".U -> AluOp.xor,
            "b0101".U -> AluOp.srl,
            "b1101".U -> AluOp.sra,
            "b0110".U -> AluOp.or,
            "b0111".U -> AluOp.and
          )
        )
        when(
          funct7 =/= "h00".U &&
            funct7 =/= "h20".U
        ) {
          decoded.illegal := true.B
        }
        when(funct7 === "h20".U && !oneOf(funct3, "b000".U, "b101".U)) {
          decoded.illegal := true.B
        }
        when(funct7 === "h00".U && funct3 === "b101".U) {
          decoded.illegal := false.B
        }
      }
    }
  }

  out := decoded
}

class Alu extends Module {
  val valid      = IO(Input(Bool()))
  val op         = IO(Input(UInt(5.W)))
  val decoded    = IO(Input(new Decoded))
  val a          = IO(Input(UInt(32.W)))
  val b          = IO(Input(UInt(32.W)))
  val result     = IO(Output(UInt(32.W)))
  val link       = IO(Output(UInt(32.W)))
  val branchTaken = IO(Output(Bool()))
  val target     = IO(Output(UInt(32.W)))
  val redirect   = IO(Output(Bool()))

  private val shamt = b(4, 0)
  private val unsignedProduct = a * b
  private val signedProduct = a.asSInt * b.asSInt
  private val signedUnsignedProduct =
    (a.asSInt.pad(64) * b.pad(64).asSInt).asUInt

  private val divByZero = b === 0.U
  private val signedOverflow = a === "h80000000".U && b === "hffffffff".U
  private val signedQuotient = a.asSInt / b.asSInt
  private val signedRemainder = a.asSInt % b.asSInt
  private val unsignedQuotient = a / b
  private val unsignedRemainder = a % b

  result := MuxLookup(op, 0.U)(
    Seq(
      AluOp.add -> (a + b),
      AluOp.sub -> (a - b),
      AluOp.sll -> (a << shamt),
      AluOp.slt -> (a.asSInt < b.asSInt).asUInt,
      AluOp.sltu -> (a < b).asUInt,
      AluOp.xor -> (a ^ b),
      AluOp.srl -> (a >> shamt),
      AluOp.sra -> (a.asSInt >> shamt).asUInt,
      AluOp.or -> (a | b),
      AluOp.and -> (a & b),
      AluOp.mul -> unsignedProduct(31, 0),
      AluOp.mulh -> signedProduct.asUInt(63, 32),
      AluOp.mulhsu -> signedUnsignedProduct(63, 32),
      AluOp.mulhu -> unsignedProduct(63, 32),
      AluOp.div -> Mux(divByZero, "hffffffff".U, Mux(signedOverflow, a, signedQuotient.asUInt)),
      AluOp.divu -> Mux(divByZero, "hffffffff".U, unsignedQuotient),
      AluOp.rem -> Mux(divByZero, a, Mux(signedOverflow, 0.U, signedRemainder.asUInt)),
      AluOp.remu -> Mux(divByZero, a, unsignedRemainder)
    )
  )

  link := decoded.pc + 4.U

  private val equal = a === b
  private val less = a.asSInt < b.asSInt
  private val lessU = a < b
  branchTaken :=
    MuxLookup(decoded.branchOp, false.B)(
      Seq(
        BranchOp.equal -> equal,
        BranchOp.notEqual -> !equal,
        BranchOp.less -> less,
        BranchOp.greaterEq -> !less,
        BranchOp.lessU -> lessU,
        BranchOp.greaterEqU -> !lessU
      )
    ) ||
      decoded.isJal ||
      decoded.isJalr

  private val pcRelativeTarget = decoded.pc + decoded.imm
  private val registerTarget = (a + decoded.imm) & "hfffffffe".U
  target := Mux(decoded.isJalr, registerTarget, pcRelativeTarget)
  redirect := valid && decoded.isControl && branchTaken
}
