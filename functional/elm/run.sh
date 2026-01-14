#!/bin/bash
# Build and run Elm nanocode

cd "$(dirname "$0")"

# Initialize if needed
if [ ! -f "elm.json" ]; then
    elm init
fi

# Compile to HTML
elm make nanocode.elm --output=nanocode.html

# Open in browser
if command -v open &> /dev/null; then
    open nanocode.html
elif command -v xdg-open &> /dev/null; then
    xdg-open nanocode.html
else
    echo "Compiled to nanocode.html - open in browser"
fi
