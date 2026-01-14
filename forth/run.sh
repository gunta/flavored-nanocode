#!/bin/bash
# Run Forth nanocode

cd "$(dirname "$0")"

gforth nanocode.fth -e bye
