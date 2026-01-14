#!/bin/bash
# Clean, build, and run Mercury nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode *.o *.mh *.err

# Build
mmc --make nanocode

# Run
./nanocode
