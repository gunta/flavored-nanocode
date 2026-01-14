#!/bin/bash
# JavaScript nanocode - choose a variant

cd "$(dirname "$0")"

echo "JavaScript variants available:"
echo "  ./node/run.sh     - Node.js"
echo "  ./browser/run.sh  - Browser (opens HTML)"
echo ""
echo "Run one of the above, or specify as argument:"
echo "  ./run.sh node"
echo "  ./run.sh browser"

if [ "$1" = "node" ]; then
    cd node && ./run.sh
elif [ "$1" = "browser" ]; then
    cd browser && ./run.sh
fi
