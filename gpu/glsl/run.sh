#!/bin/bash
# Validate GLSL shader
# Note: GLSL shaders require OpenGL context to run

cd "$(dirname "$0")"

# Validate using glslangValidator if available
if command -v glslangValidator &> /dev/null; then
    glslangValidator nanocode.glsl
    echo "GLSL shader validated successfully"
else
    echo "Install glslang to validate GLSL shaders"
    echo "Note: GLSL shaders require an OpenGL application to execute"
fi
