#!/bin/bash
# Build and run Gleam nanocode

cd "$(dirname "$0")"

# Initialize project if needed
if [ ! -f "gleam.toml" ]; then
    gleam new . --name nanocode
    cp nanocode.gleam src/nanocode.gleam
fi

gleam run
