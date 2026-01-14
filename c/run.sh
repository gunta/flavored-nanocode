#!/bin/bash
# Clean, build, and run C nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode

# Build
gcc -o nanocode nanocode.c

# Run
./nanocode
