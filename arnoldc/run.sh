#!/bin/bash
# Build and run ArnoldC nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode.jar

# Build and run
arnoldc nanocode.arnoldc
java -jar nanocode.jar
