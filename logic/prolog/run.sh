#!/bin/bash
# Run Prolog nanocode

cd "$(dirname "$0")"

# Try swipl first
if command -v swipl &> /dev/null; then
    swipl -g main -t halt nanocode.pl
elif command -v gprolog &> /dev/null; then
    gprolog --consult-file nanocode.pl --query-goal main
else
    echo "No Prolog interpreter found (swipl or gprolog)"
    exit 1
fi
