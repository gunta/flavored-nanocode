#!/bin/bash
# Run F* nanocode

cd "$(dirname "$0")"

fstar.exe nanocode.fst
