#!/bin/bash
# Run K nanocode

cd "$(dirname "$0")"

# Try various K implementations
if command -v k &> /dev/null; then
    k nanocode.k
elif command -v q &> /dev/null; then
    q nanocode.k
else
    echo "No K interpreter found"
    exit 1
fi
