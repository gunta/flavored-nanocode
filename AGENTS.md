# Repository Guidelines

## Project Structure & Module Organization
- Language flavors live in top-level folders by paradigm (e.g., `compiled/`, `interpreted/`, `functional/`, `oop/`, `systems/`, `gpu/`, `esoteric/`). Each contains single-file agents named `nanocode.<ext>`; framework-specific variants sit in subfolders (e.g., `typescript/hono/nanocode.ts`).
- Shared artifacts: `README.md` lists every flavor; `screenshot.png` shows UI output; `tests/` holds Zig-based mock server and tests.
- Add new languages in the matching paradigm folder; keep one self-contained file under ~150 lines with minimal deps.

## Build, Test, and Development Commands
- Run a flavor directly with its runtime, e.g. `python python/nanocode.py`, `node javascript/node/nanocode.mjs`, `./bash/nanocode.sh`, `deno run -A typescript/deno/nanocode.ts`.
- Tests: `tests/run_tests.sh` builds `tests/mock_server.zig`, launches it, and executes `zig test tests/mock_server_test.zig`. Requires Zig (`zig >= 0.11`) on PATH.
- Quick manual test loop: `cd tests && zig build-exe mock_server.zig -OReleaseFast -fstrip && ./mock_server &` then `zig test mock_server_test.zig`.

## Coding Style & Naming Conventions
- Maintain the single-file agent pattern named `nanocode.*`; keep tool set parity (read, write, edit, glob, grep, shell, history).
- Favor minimal dependencies; prefer stdlib. Match existing indentation per language (2 spaces for JS/TS, 4 for Python/Go/Rust/Java/C#, tabs only where idiomatic like Make).
- Keep line count ~60–130 when feasible; include concise comments for non-obvious logic. Avoid framework boilerplate unless required.

## Testing Guidelines
- Primary tests are Zig-based; ensure `tests/run_tests.sh` passes before submitting.
- For new flavors, add small smoke tests when practical (runtime-specific) and document how to run them in the PR description.

## Commit & Pull Request Guidelines
- Commit messages: short, imperative, present tense (e.g., "Add Lua flavor", "Fix bash streaming"). Avoid prefixes unless standard emerges.
- PRs should include: summary of changes, runtime/flavor affected, new deps, how to run/verify (commands and expected output). Add screenshots if UI or terminal output changes.

## Security & Configuration Tips
- Do not commit API keys. Use env vars (`ANTHROPIC_API_KEY`, `OPENROUTER_API_KEY`, optional `MODEL`) when running agents. Provide examples, not real values, in docs/tests.

## Agent-Specific Notes
- Preserve the uniform command shortcuts (`/c` clear, `/q` quit) and colorized output across flavors.
- When adding a flavor, mirror the core agentic loop and file tooling so behavior stays consistent across languages.
