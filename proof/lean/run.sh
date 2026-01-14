#!/bin/bash
# Run Lean nanocode

cd "$(dirname "$0")"

lean --run nanocode.lean
