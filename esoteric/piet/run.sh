#!/bin/bash
# Run Piet nanocode
# Note: Piet programs are images, not text files

cd "$(dirname "$0")"

echo "Piet is a visual programming language - programs are images"
echo "Use npiet or another Piet interpreter with the image file"

# Try npiet if available
if command -v npiet &> /dev/null; then
    npiet nanocode.png
fi
