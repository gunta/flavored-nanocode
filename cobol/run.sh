#!/bin/bash
# Clean, build, and run COBOL nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode

# Build
cobc -x -o nanocode nanocode.cob

# Run
./nanocode
