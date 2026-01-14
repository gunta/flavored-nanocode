#!/bin/bash
# Compile Metal shader (requires macOS)
# Note: Metal shaders typically run within an app context

cd "$(dirname "$0")"

# Compile to .air (intermediate)
xcrun -sdk macosx metal -c nanocode.metal -o nanocode.air

# Create metallib
xcrun -sdk macosx metallib nanocode.air -o nanocode.metallib

echo "Metal shader compiled to nanocode.metallib"
echo "Note: Metal shaders require a host application to execute"
