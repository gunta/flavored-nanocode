#!/bin/bash
# Check TLA+ specification

cd "$(dirname "$0")"

# Run TLC model checker
tlc nanocode.tla
