#!/bin/bash
# Build and run Odin nanocode

cd "$(dirname "$0")"

odin run nanocode.odin -file
