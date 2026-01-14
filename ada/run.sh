#!/bin/bash
# Clean, build, and run Ada nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode *.o *.ali

# Build
gnatmake nanocode.adb -o nanocode

# Run
./nanocode
