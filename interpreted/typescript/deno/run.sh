#!/bin/bash
# Run TypeScript nanocode with Deno

cd "$(dirname "$0")"

deno run --allow-all nanocode.ts
