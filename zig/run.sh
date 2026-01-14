#!/bin/bash
# Build and run Zig nanocode

cd "$(dirname "$0")"

zig run nanocode.zig
