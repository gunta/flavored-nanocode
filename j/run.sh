#!/bin/bash
# Run J nanocode

cd "$(dirname "$0")"

# Note: J files are often .ijs but we have them as part of the dir structure
jconsole < nanocode.ijs 2>/dev/null || ijconsole nanocode.ijs
