#!/bin/bash
# Compile and run Erlang nanocode

cd "$(dirname "$0")"

# Compile
erlc nanocode.erl

# Run (module name should match filename without extension)
erl -noshell -s nanocode start -s init stop
