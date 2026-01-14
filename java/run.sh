#!/bin/bash
# Clean, build, and run Java nanocode

cd "$(dirname "$0")"

# Clean
rm -f *.class

# Build and run
javac Nanocode.java
java Nanocode
