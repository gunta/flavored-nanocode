#!/bin/bash
# Build and run WebAssembly nanocode

cd "$(dirname "$0")"

# Clean
rm -f nanocode.wasm

# Compile WAT to WASM
wat2wasm nanocode.wat -o nanocode.wasm

# Run with wasmtime or wasmer
if command -v wasmtime &> /dev/null; then
    wasmtime nanocode.wasm
elif command -v wasmer &> /dev/null; then
    wasmer nanocode.wasm
else
    echo "No WASM runtime found (wasmtime or wasmer)"
    exit 1
fi
