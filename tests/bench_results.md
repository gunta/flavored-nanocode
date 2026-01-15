# Nanocode Implementation Benchmark Results

## Test Configuration
- **Iterations**: 15
- **Date**: 2026-01-15 08:47:54
- **System**: Darwin arm64
- **Mock server**: Zig (port 19283)
- **Test**: Single request/response cycle per iteration

## Results

| Implementation | Avg (ms) | Median (ms) | P95 (ms) | P99 (ms) | Min (ms) | Max (ms) | Stddev |
|----------------|----------|-------------|----------|----------|----------|----------|--------|
| Python | 95 | 94 | 103 | 103 | 88 | 103 | 4.1 |
| Go | 115 | 116 | 124 | 124 | 96 | 124 | 6.8 |
| C | 53 | 43 | 175 | 175 | 36 | 175 | 33.0 |
| Bun | 60 | 59 | 69 | 69 | 54 | 69 | 3.4 |
| Node.js | 89 | 88 | 98 | 98 | 83 | 98 | 3.6 |
| Deno | 63 | 61 | 81 | 81 | 55 | 81 | 7.6 |
| Ruby | 89 | 88 | 122 | 122 | 79 | 122 | 9.7 |
| Lua | 42 | 40 | 50 | 50 | 38 | 50 | 3.4 |
| Swift | 320 | 316 | 376 | 376 | 300 | 376 | 17.0 |
| Crystal | 72 | 44 | 471 | 471 | 29 | 471 | 107.0 |
| Zig | 40 | 42 | 45 | 45 | 31 | 45 | 4.1 |
| Assembly | 41 | 40 | 52 | 52 | 37 | 52 | 3.8 |

## Notes

- Times in milliseconds (lower is better)
- Measures full execution: startup + HTTP request + response parsing
- Each implementation runs as a separate process
- Test input: "test" followed by "/q" to quit
