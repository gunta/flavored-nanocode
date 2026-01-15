#!/bin/bash
# Clean, build, and run ARM Assembly nanocode

cd "$(dirname "$0")"

# Detect platform
if [[ "$(uname -m)" == "arm64" ]]; then
    SOURCE="nanocode-apple-silicon.s"
else
    SOURCE="nanocode.s"
fi

# Clean
rm -f nanocode nanocode.o

# Build
as -o nanocode.o "$SOURCE"
ld -o nanocode nanocode.o \
    -lSystem -framework Security -framework CoreFoundation \
    -syslibroot "$(xcrun -sdk macosx --show-sdk-path)" \
    -e _main -arch arm64

# Run
./nanocode
