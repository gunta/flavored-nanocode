# nanocode - OpenProse Edition

> A minimal agentic coding assistant written in OpenProse.
> See: https://prose.md/

## What is OpenProse?

OpenProse is a programming language where **natural language becomes executable**. A long-running AI session is a Turing-complete computer—OpenProse provides the syntax to program it.

Key concepts:
- **Sessions** — spawn AI subagents with `session "prompt"`
- **Agents** — define reusable agents with `agent name:`
- **Fourth Wall** — use `**...**` for semantic conditions the AI evaluates
- **Prose Complete** — runs on Claude Code, OpenCode, Amp, or any compatible AI session

## The Program

```prose
# nanocode - OpenProse Edition
# A minimal agentic coding assistant in OpenProse
# See: https://prose.md/

# Define the coding agent
agent coder:
  model: sonnet
  prompt: "You are a concise coding assistant. Use tools to read, write, edit files, search with glob/grep, and run shell commands. Be direct and helpful."

# Main interaction loop - keep coding until the user is satisfied
loop until **the user says goodbye or quits**:
  session: coder
    prompt: "Help the user with their coding task. Read files to understand context, make edits as needed, run commands to verify changes. Ask clarifying questions if the request is ambiguous."
```

## How It Works

In OpenProse, you don't define tools—the AI session already has them. The `session` keyword spawns a real subagent that can:

- **Read files** — examine code, configs, documentation
- **Write/edit files** — make changes to the codebase
- **Run commands** — execute shell commands, run tests
- **Search** — glob for files, grep for patterns

The `loop until **condition**:` construct uses the AI's judgment to determine when the condition is met. This is the "fourth wall" syntax—you're speaking directly to the OpenProse VM.

## Running It

### Claude Code
```bash
claude plugin marketplace add https://github.com/openprose/prose.git
claude plugin install open-prose@prose
# Then: "run nanocode.prose"
```

### OpenCode
```bash
git clone https://github.com/openprose/prose.git ~/.config/opencode/skill/open-prose
# Then: "run nanocode.prose"
```

### Amp
```bash
git clone https://github.com/openprose/prose.git ~/.config/agents/skills/open-prose
# Then: "run nanocode.prose"
```

## Why OpenProse?

Traditional implementations of nanocode require:
- HTTP client code for the Claude API
- JSON parsing for tool calls
- REPL loop implementation
- Tool execution handlers

OpenProse eliminates all of that. The AI session IS the runtime. The specification IS the implementation.

---

*In the AI era, the best code is the code you don't have to write.*
