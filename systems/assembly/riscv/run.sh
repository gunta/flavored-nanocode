#!/bin/bash
# Clean, build, and run RISC-V Assembly nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode nanocode.o

# Build (cross-compile for RISC-V)
riscv64-unknown-elf-as -o nanocode.o nanocode.s
riscv64-unknown-elf-ld -o nanocode nanocode.o

# Run with emulator (spike or qemu)
if command -v spike &> /dev/null; then
    spike pk nanocode
elif command -v qemu-riscv64 &> /dev/null; then
    qemu-riscv64 nanocode
else
    echo "No RISC-V emulator found (spike or qemu-riscv64)"
    exit 1
fi
