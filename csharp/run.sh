#!/bin/bash
# Run C# nanocode

cd "$(dirname "$0")"

# Try dotnet script first, fall back to mono
if command -v dotnet &> /dev/null; then
    dotnet script Nanocode.cs 2>/dev/null || dotnet run Nanocode.cs
elif command -v mcs &> /dev/null; then
    mcs -out:nanocode.exe Nanocode.cs
    mono nanocode.exe
else
    echo "No C# runtime found (dotnet or mono)"
    exit 1
fi
