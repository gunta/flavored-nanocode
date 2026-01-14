#!/bin/bash
# Compile HLSL shader
# Note: HLSL shaders require DirectX/Vulkan context to run

cd "$(dirname "$0")"

# Use dxc (DirectX Shader Compiler) for validation/compilation
if command -v dxc &> /dev/null; then
    dxc -T ps_6_0 -E main nanocode.hlsl
    echo "HLSL shader compiled successfully"
else
    echo "Install dxc (DirectX Shader Compiler) to compile HLSL"
    echo "Note: HLSL shaders require a DirectX application to execute"
fi
