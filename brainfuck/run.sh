#!/bin/bash
# Run Brainfuck nanocode

cd "$(dirname "$0")"

# Try various Brainfuck interpreters
if command -v bf &> /dev/null; then
    bf nanocode.bf
elif command -v brainfuck &> /dev/null; then
    brainfuck nanocode.bf
else
    echo "No Brainfuck interpreter found (bf or brainfuck)"
    exit 1
fi
