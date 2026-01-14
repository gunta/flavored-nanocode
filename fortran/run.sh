#!/bin/bash
# Clean, build, and run Fortran nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode

# Build
gfortran -o nanocode nanocode.f90

# Run
./nanocode
