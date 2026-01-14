#!/bin/bash
# Run BASIC nanocode

cd "$(dirname "$0")"

# Try various BASIC interpreters
if command -v qbasic &> /dev/null; then
    qbasic nanocode.bas
elif command -v freebasic &> /dev/null; then
    fbc nanocode.bas && ./nanocode
elif command -v bwbasic &> /dev/null; then
    bwbasic nanocode.bas
else
    echo "No BASIC interpreter found"
    exit 1
fi
