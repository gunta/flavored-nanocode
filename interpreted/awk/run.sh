#!/bin/bash
# Run AWK nanocode

cd "$(dirname "$0")"

awk -f nanocode.awk
