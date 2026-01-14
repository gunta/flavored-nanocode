#!/bin/bash
# Build and run Shakespeare nanocode

cd "$(dirname "$0")"

# Try spl2c (Shakespeare to C compiler)
spl2c < nanocode.spl > nanocode.c
gcc -o nanocode nanocode.c -lspl
./nanocode
