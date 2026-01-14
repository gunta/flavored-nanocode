#!/bin/bash
# Run Smalltalk nanocode

cd "$(dirname "$0")"

# Try gst (GNU Smalltalk)
gst nanocode.st
