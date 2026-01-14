#!/bin/bash
# Run Common Lisp nanocode

cd "$(dirname "$0")"

# Try sbcl first, fall back to clisp
if command -v sbcl &> /dev/null; then
    sbcl --script nanocode.lisp
elif command -v clisp &> /dev/null; then
    clisp nanocode.lisp
else
    echo "No Lisp interpreter found (sbcl or clisp)"
    exit 1
fi
