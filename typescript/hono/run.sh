#!/bin/bash
# Run TypeScript Hono web server

cd "$(dirname "$0")"

# Initialize if needed
if [ ! -f "package.json" ]; then
    npm init -y
    npm install hono @hono/node-server
fi

npx tsx nanocode.ts
