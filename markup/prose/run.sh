#!/bin/bash
# Display prose nanocode (markdown)

cd "$(dirname "$0")"

# Display with glow if available, otherwise cat
if command -v glow &> /dev/null; then
    glow nanocode.md
elif command -v bat &> /dev/null; then
    bat nanocode.md
else
    cat nanocode.md
fi
