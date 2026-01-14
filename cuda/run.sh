#!/bin/bash
# Clean, build, and run CUDA nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode

# Build
nvcc -o nanocode nanocode.cu

# Run
./nanocode
