#!/bin/bash
# Clean, build, and run Pascal nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode *.o

# Build
fpc -o nanocode nanocode.pas

# Run
./nanocode
