#!/bin/bash
# Clean, build, and run C++ nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode

# Build
g++ -std=c++17 -o nanocode nanocode.cpp

# Run
./nanocode
