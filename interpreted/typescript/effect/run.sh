#!/bin/bash
# Run TypeScript Effect nanocode

cd "$(dirname "$0")"

# Initialize if needed
if [ ! -f "package.json" ]; then
    npm init -y
    npm install effect
fi

npx tsx nanocode.ts
