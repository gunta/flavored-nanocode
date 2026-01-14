#!/bin/bash
# Clean, build, and run D nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode nanocode.o

# Build and run
dmd -of=nanocode nanocode.d
./nanocode
