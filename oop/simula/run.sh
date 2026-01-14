#!/bin/bash
# Run Simula nanocode

cd "$(dirname "$0")"

# Try cim (GNU Simula)
cim nanocode.sim
./nanocode
