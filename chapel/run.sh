#!/bin/bash
# Clean, build, and run Chapel nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode

# Build
chpl -o nanocode nanocode.chpl

# Run
./nanocode
