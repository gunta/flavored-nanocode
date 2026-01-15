#!/bin/bash
cd "$(dirname "$0")"

# Build with optimizations (69KB binary)
zig build-exe nanocode.zig -OReleaseFast -fstrip 2>&1

./nanocode
