#!/bin/bash
# Benchmark runner for nanocode implementations
# Tests actual nanocode clients against the Zig mock server
#
# Usage: ./bench.sh [iterations]

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
ITERATIONS="${1:-10}"
PORT=19283
RESULTS_FILE="${SCRIPT_DIR}/bench_results.md"
SERVER_PID=""

# Colors
BOLD='\033[1m'
DIM='\033[2m'
GREEN='\033[32m'
BLUE='\033[34m'
CYAN='\033[36m'
RED='\033[31m'
YELLOW='\033[33m'
RESET='\033[0m'

stop_server() {
    if [ -n "$SERVER_PID" ]; then
        kill "$SERVER_PID" 2>/dev/null || true
        wait "$SERVER_PID" 2>/dev/null || true
        SERVER_PID=""
    fi
}

cleanup_port() {
    lsof -ti:${PORT} 2>/dev/null | xargs kill -9 2>/dev/null || true
    sleep 0.3
}

trap stop_server EXIT

echo -e "${BOLD}${CYAN}╭────────────────────────────────────────╮${RESET}"
echo -e "${BOLD}${CYAN}│    nanocode implementation benchmark   │${RESET}"
echo -e "${BOLD}${CYAN}╰────────────────────────────────────────╯${RESET}"
echo -e "${DIM}Iterations: ${ITERATIONS} | Port: ${PORT}${RESET}"
echo -e "${DIM}Testing nanocode clients against Zig mock server${RESET}\n"

cleanup_port

# Build Zig benchmark server (simple end_turn responses only)
if [ ! -f "${SCRIPT_DIR}/bench_server" ] || [ "${SCRIPT_DIR}/bench_server.zig" -nt "${SCRIPT_DIR}/bench_server" ]; then
    echo -e "${BLUE}▸${RESET} Building Zig benchmark server..."
    (cd "$SCRIPT_DIR" && zig build-exe bench_server.zig -O ReleaseFast -fstrip -femit-bin=bench_server 2>/dev/null || \
     cd "$SCRIPT_DIR" && zig build-exe bench_server.zig -O ReleaseFast -femit-bin=bench_server)
    echo -e "${GREEN}✓${RESET} Benchmark server built"
fi

# Start benchmark server
echo -e "${BLUE}▸${RESET} Starting benchmark server..."
"${SCRIPT_DIR}/bench_server" > /dev/null 2>&1 &
SERVER_PID=$!
sleep 0.5

if ! kill -0 "$SERVER_PID" 2>/dev/null; then
    echo -e "${RED}✗ Failed to start benchmark server${RESET}"
    exit 1
fi
echo -e "${GREEN}✓${RESET} Benchmark server running (PID: $SERVER_PID)\n"

# Environment for testing
export API_URL="http://localhost:${PORT}/v1/messages"
export ANTHROPIC_API_KEY="mock-key"
export MODEL="mock-model"

# Array to store results
declare -a RESULTS=()

bench_impl() {
    local name="$1"
    local cmd="$2"
    
    echo -e "${BLUE}▸${RESET} Benchmarking ${name}..."
    
    local times=()
    local success=0
    
    for ((i=1; i<=ITERATIONS; i++)); do
        # Time the execution
        local start=$(python3 -c 'import time; print(time.time())')
        
        # Run with timeout, pipe "test" as input, then "/q" to quit
        if echo -e "test\n/q" | timeout 10 $cmd > /dev/null 2>&1; then
            local end=$(python3 -c 'import time; print(time.time())')
            local elapsed=$(python3 -c "print(round(($end - $start) * 1000, 2))")
            times+=("$elapsed")
            ((success++))
        fi
    done
    
    if [ $success -eq 0 ]; then
        echo -e "${RED}✗ ${name} failed${RESET}\n"
        return 1
    fi
    
    # Calculate stats using Python
    local stats=$(python3 << EOF
import json
times = [${times[@]/%/,}]
if not times:
    print('{}')
else:
    times.sort()
    n = len(times)
    avg = sum(times) / n
    median = times[n // 2]
    min_t = times[0]
    max_t = times[-1]
    p95 = times[int(n * 0.95)] if n > 1 else times[0]
    p99 = times[int(n * 0.99)] if n > 1 else times[0]
    stddev = (sum((t - avg) ** 2 for t in times) / n) ** 0.5
    print(json.dumps({
        "language": "${name}",
        "iterations": $success,
        "avg_ms": round(avg, 2),
        "median_ms": round(median, 2),
        "min_ms": round(min_t, 2),
        "max_ms": round(max_t, 2),
        "p95_ms": round(p95, 2),
        "p99_ms": round(p99, 2),
        "stddev_ms": round(stddev, 2),
        "requests_per_sec": round(1000 / avg, 0) if avg > 0 else 0
    }))
EOF
)
    
    if [ -n "$stats" ] && [ "$stats" != "{}" ]; then
        echo -e "${GREEN}✓${RESET} ${name} completed (${success}/${ITERATIONS})"
        echo "$stats" | python3 -c "import sys,json; d=json.load(sys.stdin); print(f'  avg: {d[\"avg_ms\"]:.0f}ms | median: {d[\"median_ms\"]:.0f}ms | p99: {d[\"p99_ms\"]:.0f}ms')"
        RESULTS+=("$stats")
    else
        echo -e "${RED}✗ ${name} no successful runs${RESET}"
    fi
    echo ""
}

echo -e "${YELLOW}═══════════════════════════════════════════${RESET}\n"

# Test implementations that support API_URL
# Python
if command -v python3 &> /dev/null; then
    bench_impl "Python" "python3 ${ROOT_DIR}/interpreted/python/nanocode.py"
fi

# Go
if command -v go &> /dev/null; then
    bench_impl "Go" "go run ${ROOT_DIR}/compiled/go/nanocode.go"
fi

# Rust (via rust-script if available, or rustc)
if command -v rust-script &> /dev/null; then
    bench_impl "Rust" "rust-script ${ROOT_DIR}/compiled/rust/nanocode.rs"
elif command -v rustc &> /dev/null; then
    # Compile first
    if rustc -O -o /tmp/nanocode_rust "${ROOT_DIR}/compiled/rust/nanocode.rs" 2>/dev/null; then
        bench_impl "Rust" "/tmp/nanocode_rust"
    fi
fi

# C
if command -v clang &> /dev/null || command -v gcc &> /dev/null; then
    CC=${CC:-clang}
    if $CC -O2 -o /tmp/nanocode_c "${ROOT_DIR}/compiled/c/nanocode.c" -lcurl 2>/dev/null; then
        bench_impl "C" "/tmp/nanocode_c"
    fi
fi

# Bun
if command -v bun &> /dev/null; then
    bench_impl "Bun" "bun run ${ROOT_DIR}/interpreted/typescript/bun/nanocode.ts"
fi

# Node.js
if command -v node &> /dev/null; then
    bench_impl "Node.js" "node ${ROOT_DIR}/interpreted/javascript/node/nanocode.mjs"
fi

# Deno
if command -v deno &> /dev/null; then
    bench_impl "Deno" "deno run -A ${ROOT_DIR}/interpreted/typescript/deno/nanocode.ts"
fi

# Ruby
if command -v ruby &> /dev/null; then
    bench_impl "Ruby" "ruby ${ROOT_DIR}/interpreted/ruby/nanocode.rb"
fi

# Lua (needs luasocket and lua-cjson)
if command -v lua &> /dev/null; then
    if lua -e "require('cjson'); require('socket.http')" 2>/dev/null; then
        bench_impl "Lua" "lua ${ROOT_DIR}/interpreted/lua/nanocode.lua"
    fi
fi

# Swift
if command -v swift &> /dev/null; then
    bench_impl "Swift" "swift ${ROOT_DIR}/compiled/swift/nanocode.swift"
fi

# Nim - Skipped: HTTP client has issues with simple mock server
# if command -v nim &> /dev/null; then
#     nim c -d:release -d:ssl --hints:off -o:/tmp/nanocode_nim "${ROOT_DIR}/compiled/nim/nanocode.nim" 2>/dev/null
#     [ -f /tmp/nanocode_nim ] && bench_impl "Nim" "/tmp/nanocode_nim"
# fi

# Crystal (pre-compile for fair comparison)
if command -v crystal &> /dev/null; then
    if [ ! -f /tmp/nanocode_crystal ] || [ "${ROOT_DIR}/compiled/crystal/nanocode.cr" -nt /tmp/nanocode_crystal ]; then
        echo -e "${BLUE}▸${RESET} Building Crystal..."
        crystal build --release -o /tmp/nanocode_crystal "${ROOT_DIR}/compiled/crystal/nanocode.cr" 2>/dev/null
    fi
    if [ -f /tmp/nanocode_crystal ]; then
        bench_impl "Crystal" "/tmp/nanocode_crystal"
    fi
fi

# Zig
if [ -f "${ROOT_DIR}/zig/nanocode" ]; then
    bench_impl "Zig" "${ROOT_DIR}/zig/nanocode"
elif command -v zig &> /dev/null && [ -f "${ROOT_DIR}/zig/nanocode.zig" ]; then
    echo -e "${BLUE}▸${RESET} Building Zig..."
    (cd "${ROOT_DIR}/zig" && zig build-exe nanocode.zig -O ReleaseFast -fstrip 2>/dev/null)
    if [ -f "${ROOT_DIR}/zig/nanocode" ]; then
        bench_impl "Zig" "${ROOT_DIR}/zig/nanocode"
    fi
fi

# Assembly (ARM64 only, uses port 8080)
if [[ "$(uname -m)" == "arm64" ]] && [ -f "${ROOT_DIR}/systems/assembly/arm/nanocode" ]; then
    # Start bench server on port 8080 for assembly (uses different port)
    stop_server
    "${SCRIPT_DIR}/bench_server" 8080 > /dev/null 2>&1 &
    SERVER_PID=$!
    sleep 0.5
    
    if kill -0 "$SERVER_PID" 2>/dev/null; then
        # Assembly uses API_URL env to detect test mode (port 8080)
        OLD_API_URL="$API_URL"
        export API_URL="http://localhost:8080"
        bench_impl "Assembly" "${ROOT_DIR}/systems/assembly/arm/nanocode"
        export API_URL="$OLD_API_URL"
    fi
    
    # Restart main bench server on port 19283
    stop_server
    "${SCRIPT_DIR}/bench_server" > /dev/null 2>&1 &
    SERVER_PID=$!
    sleep 0.5
fi

echo -e "${YELLOW}═══════════════════════════════════════════${RESET}"

# Generate markdown results
echo -e "\n${BOLD}${CYAN}╭────────────────────────────────────────╮${RESET}"
echo -e "${BOLD}${CYAN}│              Results                   │${RESET}"
echo -e "${BOLD}${CYAN}╰────────────────────────────────────────╯${RESET}\n"

cat > "$RESULTS_FILE" << EOF
# Nanocode Implementation Benchmark Results

## Test Configuration
- **Iterations**: ${ITERATIONS}
- **Date**: $(date '+%Y-%m-%d %H:%M:%S')
- **System**: $(uname -s) $(uname -m)
- **Mock server**: Zig (port ${PORT})
- **Test**: Single request/response cycle per iteration

## Results

| Implementation | Avg (ms) | Median (ms) | P95 (ms) | P99 (ms) | Min (ms) | Max (ms) | Stddev |
|----------------|----------|-------------|----------|----------|----------|----------|--------|
EOF

for result in "${RESULTS[@]}"; do
    if [ -n "$result" ]; then
        echo "$result" | python3 -c "
import sys, json
d = json.load(sys.stdin)
print(f\"| {d['language']} | {d['avg_ms']:.0f} | {d['median_ms']:.0f} | {d['p95_ms']:.0f} | {d['p99_ms']:.0f} | {d['min_ms']:.0f} | {d['max_ms']:.0f} | {d['stddev_ms']:.1f} |\")" >> "$RESULTS_FILE" 2>/dev/null
    fi
done

cat >> "$RESULTS_FILE" << 'EOF'

## Notes

- Times in milliseconds (lower is better)
- Measures full execution: startup + HTTP request + response parsing
- Each implementation runs as a separate process
- Test input: "test" followed by "/q" to quit
EOF

echo -e "${BOLD}Results saved to: ${RESULTS_FILE}${RESET}\n"
cat "$RESULTS_FILE"

echo -e "\n${GREEN}✓ Benchmark complete${RESET}"
