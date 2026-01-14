#!/bin/bash
# Clean, build, and run Vale nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode build/*

# Build
vale build nanocode.vale -o nanocode

# Run
./nanocode
