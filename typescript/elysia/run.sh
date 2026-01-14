#!/bin/bash
# Run TypeScript Elysia web server (Bun)

cd "$(dirname "$0")"

# Initialize if needed
if [ ! -f "package.json" ]; then
    bun init -y
    bun add elysia
fi

bun run nanocode.ts
