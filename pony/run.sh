#!/bin/bash
# Clean, build, and run Pony nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode

# Build and run (pony compiles to current dir)
ponyc -o . -b nanocode .
./nanocode
