#!/bin/bash
# Run ALGOL nanocode

cd "$(dirname "$0")"

# Try algol68g (ALGOL 68 Genie)
a68g nanocode.alg
