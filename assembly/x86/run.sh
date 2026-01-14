#!/bin/bash
# Clean, build, and run x86 Assembly nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode nanocode.o

# Build (using NASM for x86)
nasm -f elf64 -o nanocode.o nanocode.asm
ld -o nanocode nanocode.o

# Run
./nanocode
