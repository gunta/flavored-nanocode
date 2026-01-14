#!/bin/bash
# Build and run Jai nanocode
# Note: Jai is not publicly available yet

cd "$(dirname "$0")"

# Jai compiler (when available)
if command -v jai &> /dev/null; then
    jai nanocode.jai -run
else
    echo "Jai compiler not available (closed beta)"
    echo "See: https://jai.community"
fi
