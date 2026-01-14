# nanocode - BetterScript

[BetterScript](https://github.com/code-agents/betterscript) is an LLM-first language with deterministic semantics that compiles to ReScript and TypeScript.

## Features Demonstrated

- **Variant Types**: `contentBlock` uses sum types for Text/ToolUse discrimination
- **Pattern Matching**: Exhaustive `switch` expressions for control flow
- **Option Chaining**: Safe nullable handling with `Option.flatMap`, `Option.getWithDefault`
- **Pipe Operators**: `->` for left-to-right data transformation
- **Async/Await**: Native async syntax for effectful operations
- **Refs**: Mutable state with `ref` type for message history
- **Recursive Functions**: `rec` keyword for REPL and agent loops
- **Record Types**: Structured data with optional `@as` attribute for JSON keys

## BetterScript Concepts

BetterScript extends ReScript with LLM-centric features:

- **Deterministic Semantics**: Predictable execution for agent workflows
- **Effect Tracking**: Explicit side effect management
- **Type-safe JSON**: Schema validation at compile time
- **Tool Definitions**: First-class support for LLM tool schemas

## Setup

```bash
# Install BetterScript compiler
bun add betterscript

# Compile to TypeScript/ReScript
betterscript compile nanocode.bs

# Run compiled output
bun run nanocode.bs.mjs
```

## Environment Variables

```bash
export ANTHROPIC_API_KEY="your-key"
export MODEL="claude-sonnet-4-20250514"  # optional
```
