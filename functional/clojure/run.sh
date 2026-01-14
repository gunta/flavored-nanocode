#!/bin/bash
# Run Clojure nanocode

cd "$(dirname "$0")"

# Try clj first, fall back to clojure
if command -v clj &> /dev/null; then
    clj -M nanocode.clj
elif command -v clojure &> /dev/null; then
    clojure nanocode.clj
else
    echo "No Clojure runtime found"
    exit 1
fi
