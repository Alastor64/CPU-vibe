package cpu2026

/** Synthesizable configuration of the processor.
  *
  * Keep every architectural knob in this immutable object so generated
  * hardware, documentation, and test matrices cannot silently diverge.
  */
final case class CpuConfig(
    issueWidth:         Int = 1,
    robEntries:         Int = 16,
    physicalRegisters:  Int = 64,
    issueQueueEntries:  Int = 8,
    icacheBytes:        Int = 4096,
    dcacheBytes:        Int = 4096,
    cacheWays:          Int = 2,
    cacheLineBytes:     Int = 64
) {
  require(CpuConfig.supportedIssueWidths.contains(issueWidth), "issueWidth must be 1, 2, or 4")
  require(isPowerOfTwo(robEntries), "robEntries must be a positive power of two")
  require(physicalRegisters >= 32, "physicalRegisters must include all 32 architectural registers")
  require(isPowerOfTwo(physicalRegisters), "physicalRegisters must be a power of two")
  require(isPowerOfTwo(issueQueueEntries), "issueQueueEntries must be a positive power of two")
  require(isPowerOfTwo(icacheBytes) && icacheBytes >= 256, "icacheBytes must be a power of two and at least 256")
  require(isPowerOfTwo(dcacheBytes) && dcacheBytes >= 256, "dcacheBytes must be a power of two and at least 256")
  require(isPowerOfTwo(cacheWays), "cacheWays must be a positive power of two")
  require(
    isPowerOfTwo(cacheLineBytes) && cacheLineBytes >= 16,
    "cacheLineBytes must be a power of two and at least 16"
  )

  private def isPowerOfTwo(value: Int): Boolean = value > 0 && (value & (value - 1)) == 0
}

object CpuConfig {
  val supportedIssueWidths: Seq[Int] = Seq(1, 2, 4)
}
