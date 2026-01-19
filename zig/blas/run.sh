#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")"

if [[ "$(uname)" != "Darwin" ]]; then
  echo "This flavor links macOS Accelerate BLAS (Darwin only)." >&2
  exit 1
fi

zig build-exe nanocode.zig -OReleaseFast -fstrip -framework Accelerate 2>&1
./nanocode
