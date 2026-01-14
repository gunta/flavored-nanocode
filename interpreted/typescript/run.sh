#!/bin/bash
# TypeScript nanocode - choose a variant

cd "$(dirname "$0")"

echo "TypeScript variants available:"
echo "  ./node/run.sh    - Node.js with tsx"
echo "  ./bun/run.sh     - Bun runtime"
echo "  ./deno/run.sh    - Deno runtime"
echo "  ./hono/run.sh    - Hono web framework"
echo "  ./elysia/run.sh  - Elysia web framework (Bun)"
echo "  ./effect/run.sh  - Effect library"
echo "  ./aisdk/run.sh   - AI SDK"
echo ""
echo "Run one of the above, or specify as argument:"
echo "  ./run.sh node"
echo "  ./run.sh bun"

case "$1" in
    node)   cd node && ./run.sh ;;
    bun)    cd bun && ./run.sh ;;
    deno)   cd deno && ./run.sh ;;
    hono)   cd hono && ./run.sh ;;
    elysia) cd elysia && ./run.sh ;;
    effect) cd effect && ./run.sh ;;
    aisdk)  cd aisdk && ./run.sh ;;
esac
