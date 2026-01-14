#!/bin/bash
# Run Scheme nanocode

cd "$(dirname "$0")"

# Try various Scheme implementations
if command -v guile &> /dev/null; then
    guile nanocode.scm
elif command -v chicken-csi &> /dev/null; then
    chicken-csi -s nanocode.scm
elif command -v racket &> /dev/null; then
    racket nanocode.scm
else
    echo "No Scheme interpreter found (guile, chicken, or racket)"
    exit 1
fi
