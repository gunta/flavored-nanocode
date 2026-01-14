#!/bin/bash
# Run Agda nanocode

cd "$(dirname "$0")"

agda --compile nanocode.agda && ./nanocode
