#!/bin/bash
# Assembly nanocode - choose a variant

cd "$(dirname "$0")"

echo "Assembly variants available:"
echo "  ./arm/run.sh   - ARM64/Apple Silicon"
echo "  ./x86/run.sh   - x86-64"
echo "  ./riscv/run.sh - RISC-V"
echo "  ./simd/run.sh  - SIMD extensions"
echo ""
echo "Run one of the above, or specify as argument:"
echo "  ./run.sh arm"
echo "  ./run.sh x86"

case "$1" in
    arm)   cd arm && ./run.sh ;;
    x86)   cd x86 && ./run.sh ;;
    riscv) cd riscv && ./run.sh ;;
    simd)  cd simd && ./run.sh ;;
esac
