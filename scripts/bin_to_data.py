#!/usr/bin/env python3
"""Convert a flat RV32 binary image to the course program.data format."""

from pathlib import Path
import sys


def main() -> int:
    if len(sys.argv) != 3:
        print("usage: bin_to_data.py INPUT.bin OUTPUT.data", file=sys.stderr)
        return 2
    image = Path(sys.argv[1]).read_bytes()
    lines = ["# Byte-addressed flat image."]
    for offset in range(0, len(image), 16):
        chunk = image[offset : offset + 16]
        if offset % 256 == 0:
            lines.append(f"@{offset:08x}")
        lines.append(" ".join(f"{byte:02x}" for byte in chunk))
    Path(sys.argv[2]).write_text("\n".join(lines) + "\n", encoding="ascii")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
