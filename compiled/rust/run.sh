#!/bin/bash
# Clean, build, and run Rust nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode

# Build
rustc -o nanocode nanocode.rs

# Run
./nanocode
