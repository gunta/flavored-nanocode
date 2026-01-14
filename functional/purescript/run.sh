#!/bin/bash
# Build and run PureScript nanocode

cd "$(dirname "$0")"

# Initialize if needed
if [ ! -f "spago.yaml" ]; then
    spago init
fi

spago run
