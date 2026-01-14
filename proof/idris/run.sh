#!/bin/bash
# Run Idris nanocode

cd "$(dirname "$0")"

idris2 --exec main nanocode.idr
