#!/bin/bash
# Run Whitespace nanocode

cd "$(dirname "$0")"

# Try wspace or other whitespace interpreters
if command -v wspace &> /dev/null; then
    wspace nanocode.ws
elif command -v whitespace &> /dev/null; then
    whitespace nanocode.ws
else
    echo "No Whitespace interpreter found"
    exit 1
fi
