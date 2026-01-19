#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")"

if [[ "$(uname -s)" != "Darwin" ]] || [[ "$(uname -m)" != "arm64" ]]; then
  echo "This flavor is macOS arm64 only (Apple Silicon + Accelerate BLAS)." >&2
  exit 1
fi

rm -f nanocode nanocode.o
as -o nanocode.o nanocode.s
ld -o nanocode nanocode.o \
  -lSystem -framework Accelerate \
  -syslibroot "$(xcrun -sdk macosx --show-sdk-path)" \
  -e _main -arch arm64

./nanocode
