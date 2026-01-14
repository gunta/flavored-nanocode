#!/bin/bash
# Run Befunge-93 nanocode

cd "$(dirname "$0")"

# Try cfunge or bef
if command -v cfunge &> /dev/null; then
    cfunge nanocode.bf93
elif command -v bef &> /dev/null; then
    bef nanocode.bf93
else
    echo "No Befunge interpreter found (cfunge or bef)"
    exit 1
fi
