#!/bin/bash
# Run Visual Basic nanocode

cd "$(dirname "$0")"

# Try dotnet first, fall back to mono
if command -v dotnet &> /dev/null; then
    dotnet run nanocode.vb
elif command -v vbnc &> /dev/null; then
    vbnc -out:nanocode.exe nanocode.vb
    mono nanocode.exe
else
    echo "No VB runtime found (dotnet or mono)"
    exit 1
fi
