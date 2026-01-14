#!/bin/bash
# Build and run INTERCAL nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode nanocode.c

# Build (using C-INTERCAL)
ick nanocode.i
./nanocode
