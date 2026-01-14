#!/bin/bash
# Run Literate Haskell nanocode

cd "$(dirname "$0")"

runhaskell nanocode.lhs
