#!/bin/bash
# Clean, build, and run Nim nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode

# Build and run
nim compile --run nanocode.nim
