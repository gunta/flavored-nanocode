#!/bin/bash
# Build and run Emojicode nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode

# Build
emojicodec nanocode.emojic

# Run
./nanocode
