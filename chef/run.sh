#!/bin/bash
# Run Chef nanocode

cd "$(dirname "$0")"

# Chef interpreter
chef nanocode.chef
