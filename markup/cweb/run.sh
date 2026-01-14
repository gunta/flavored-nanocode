#!/bin/bash
# Build and run CWEB nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode nanocode.c nanocode.tex

# Extract C code from CWEB
ctangle nanocode.w

# Build
gcc -o nanocode nanocode.c

# Run
./nanocode
