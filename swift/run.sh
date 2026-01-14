#!/bin/bash
# Clean, build, and run Swift nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode

# Build
swiftc -o nanocode nanocode.swift

# Run
./nanocode
