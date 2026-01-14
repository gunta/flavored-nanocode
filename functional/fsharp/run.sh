#!/bin/bash
# Run F# nanocode

cd "$(dirname "$0")"

dotnet fsi nanocode.fsx
