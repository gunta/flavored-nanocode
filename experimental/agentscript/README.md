# nanocode - AgentScript

An implementation of nanocode in [AgentScript](https://github.com/code-agents/agentscript), an AI-first intermediate language designed for robust, testable code generation.

## What is AgentScript?

AgentScript bridges AI-generated code and human-readable syntax. It's designed to be:

- **AI-First**: Minimal token usage for cost-effective generation
- **Contract Programming**: Built-in `@requires` constraints for input validation
- **Embedded Testing**: Tests live alongside code with `@test` annotations
- **Transpilable**: Outputs to TypeScript with test suites

## Features Demonstrated

This implementation showcases AgentScript's key features:

### Contract Programming
```agentscript
/**
 * @requires path.length > 0: "Path cannot be empty"
 * @requires offset >= 0: "Offset must be non-negative"
 */
function readFile(args: { path: string, offset?: number }): string
```

### Embedded Testing
```agentscript
/**
 * @test { path: "test.txt" } -> "1| hello\n2| world"
 * @test { path: "" } -> FileNotFound
 * @performance { path: "small.txt" } -> expect < 10ms
 */
```

### Pattern Matching
```agentscript
match (block.type) {
  case "text" -> console.log(block.text)
  case "tool_use" -> executeTool(block.name, block.input)
}
```

### State Machines
```agentscript
machine AgentMachine {
  initial: idle
  state idle { on USER_INPUT -> processing }
  state processing { on API_RESPONSE -> idle }
}
```

### Pipe Operators
```agentscript
const files = fs.glob(pattern)
  |> sortBy(f => fs.mtime(f), "desc")
  |> take(50)
  |> join("\n")
```

## Transpilation

AgentScript transpiles to TypeScript:

```bash
# Conceptual (transpiler in development)
agentscript compile nanocode.ags -o nanocode.ts
```

The generated TypeScript includes:
- Runtime contract validation
- Vitest test suites
- k6 performance tests
- XState state machine definitions

## Environment

```bash
export ANTHROPIC_API_KEY="your-key"
export MODEL="claude-sonnet-4-20250514"  # optional
```

## Why AgentScript for AI Agents?

1. **Self-Documenting**: Every function carries its own tests and contracts
2. **AI-Readable**: Structured metadata helps LLMs understand intent
3. **Fail-Fast**: Contract violations caught early with clear errors
4. **Concurrency-Aware**: State machines make async flows explicit

## Reference

- [AgentScript Repository](https://github.com/code-agents/agentscript)
- [Language Specification](https://github.com/code-agents/agentscript/tree/main/papers/agentscript-lang)
