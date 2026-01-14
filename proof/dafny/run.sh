#!/bin/bash
# Run Dafny nanocode

cd "$(dirname "$0")"

dafny run nanocode.dfy
