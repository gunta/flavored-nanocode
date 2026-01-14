#!/bin/bash
# Run TypeScript nanocode with Node.js

cd "$(dirname "$0")"

# Try tsx first (faster), fall back to ts-node
if command -v tsx &> /dev/null; then
    tsx nanocode.ts
elif command -v ts-node &> /dev/null; then
    ts-node nanocode.ts
else
    npx tsx nanocode.ts
fi
