#!/bin/bash
# Run Koka nanocode

cd "$(dirname "$0")"

koka -e nanocode.kk
