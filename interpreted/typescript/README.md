# TypeScript Nanocode

Multiple flavors for different runtimes.

## Node.js (zero deps)

```bash
node --experimental-strip-types node/nanocode.ts
```

## Bun (zero deps)

```bash
bun bun/nanocode.ts
```

## Deno (zero deps)

```bash
deno run -A deno/nanocode.ts
```

## AI SDK (requires @ai-sdk/anthropic, ai, zod)

```bash
bun add @ai-sdk/anthropic ai zod
bun aisdk/nanocode.ts
```
