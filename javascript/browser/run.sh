#!/bin/bash
# Open JavaScript nanocode in browser

cd "$(dirname "$0")"

# Try to open in default browser
if command -v open &> /dev/null; then
    open nanocode.html
elif command -v xdg-open &> /dev/null; then
    xdg-open nanocode.html
elif command -v start &> /dev/null; then
    start nanocode.html
else
    echo "Open nanocode.html in your browser"
fi
