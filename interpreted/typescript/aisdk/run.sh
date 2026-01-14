#!/bin/bash
# Run TypeScript AI SDK nanocode

cd "$(dirname "$0")"

# Initialize if needed
if [ ! -f "package.json" ]; then
    npm init -y
    npm install ai @ai-sdk/openai
fi

npx tsx nanocode.ts
