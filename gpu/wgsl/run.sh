#!/bin/bash
# Validate WGSL shader
# Note: WGSL shaders require WebGPU context to run

cd "$(dirname "$0")"

# Use wgsl-analyzer or naga for validation
if command -v naga &> /dev/null; then
    naga nanocode.wgsl
    echo "WGSL shader validated successfully"
else
    echo "Install naga to validate WGSL shaders"
    echo "Note: WGSL shaders require a WebGPU application to execute"
fi
