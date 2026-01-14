#!/bin/bash
# Clean, build, and run SIMD Assembly nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode nanocode.o

# Build (using NASM with SIMD extensions)
nasm -f elf64 -o nanocode.o nanocode.asm
ld -o nanocode nanocode.o

# Run
./nanocode
