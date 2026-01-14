#!/bin/bash
# Run Haskell nanocode

cd "$(dirname "$0")"

# Try runhaskell first, fall back to ghc compile + run
if command -v runhaskell &> /dev/null; then
    runhaskell nanocode.hs
else
    ghc -o nanocode nanocode.hs
    ./nanocode
fi
