#!/bin/bash
# Clean, build, and run Modula-2 nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode *.o

# Build (using GNU Modula-2 or gm2)
gm2 -o nanocode nanocode.mod

# Run
./nanocode
