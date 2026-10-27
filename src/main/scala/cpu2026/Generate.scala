package cpu2026

import circt.stage.ChiselStage
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/** Emit one self-contained SystemVerilog file for submission.
  *
  * Usage: `scala-cli run . -- src/main/scala/cpu2026/Generate.scala -- output.sv`
  * The default output is ignored by Git because CPU-submit owns generated RTL.
  */
object Generate {
  def main(args: Array[String]): Unit = {
    require(args.length <= 1, "usage: Generate [systemverilog-output-path]")
    val output = Path
      .of(args.headOption.getOrElse("build/verilog/student_top.sv"))
      .toAbsolutePath
      .normalize()

    Option(output.getParent).foreach(Files.createDirectories(_))
    val systemVerilog = ChiselStage.emitSystemVerilog(
      new StudentTop,
      firtoolOpts = Array(
        "--disable-all-randomization",
        "--strip-debug-info",
        "--default-layer-specialization=enable",
        "--verification-flavor=if-else-fatal",
        "--lowering-options=disallowLocalVariables,disallowPackedArrays"
      )
    )
    // Keep generated files stable under `git diff --check` and repeated builds.
    Files.writeString(output, systemVerilog.stripTrailing() + "\n", StandardCharsets.UTF_8)
    println(s"Generated $output")
  }
}
