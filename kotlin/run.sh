#!/bin/bash
# Clean, build, and run Kotlin nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode.jar

# Build and run
kotlinc nanocode.kt -include-runtime -d nanocode.jar
java -jar nanocode.jar
