#!/bin/bash
# Run APL nanocode

cd "$(dirname "$0")"

# Try Dyalog APL or GNU APL
if command -v dyalog &> /dev/null; then
    dyalog -script nanocode.apl
elif command -v apl &> /dev/null; then
    apl -f nanocode.apl
else
    echo "No APL interpreter found (dyalog or gnu-apl)"
    exit 1
fi
