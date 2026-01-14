# ARM Assembly nanocode

| File | Target | Description |
|------|--------|-------------|
| `nanocode.s` | ARM64 Linux | Standard Linux ARM64 implementation |
| `nanocode-apple-silicon.s` | M4/M5 Pro Max | **CPU-optimized** - SIMD, cache hints, keep-alive |

---

## Apple Silicon M4/M5 Pro Max - CPU-Optimized

Uses CPU-side optimizations that matter for this workload:

```bash
# Build
as -o nanocode.o nanocode-apple-silicon.s
ld -o nanocode nanocode.o -lSystem -framework Security -framework CoreFoundation \
   -syslibroot `xcrun -sdk macosx --show-sdk-path` -e _main -arch arm64

# Run
export ANTHROPIC_API_KEY=sk-ant-...
./nanocode
```

---

## CPU Features Utilized

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                    M4/M5 Pro Max CPU Utilization                             │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                              │
│  ┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐         │
│  │  NEON SIMD      │    │  P-Core Cache   │    │  Branch Unit    │         │
│  │  128-bit vectors│    │  Large L1 cache │    │  Wide decode    │         │
│  │  v0-v31 regs    │    │  Cache-line align│   │  Modern predictor│         │
│  └────────┬────────┘    └────────┬────────┘    └────────┬────────┘         │
│           │                      │                      │                   │
│           ▼                      ▼                      ▼                   │
│  ┌─────────────────────────────────────────────────────────────────┐       │
│  │                    nanocode optimizations                        │       │
│  ├─────────────────────────────────────────────────────────────────┤       │
│  │  • strlen_simd: 16 bytes/cycle null scan with CMEQ+UMAXV        │       │
│  │  • strcpy_simd: 16-byte LDR Q/STR Q block copies                │       │
│  │  • PRFM prefetch 256 bytes ahead (pldl1strm, pstl1strm)         │       │
│  │  • 128-byte cache line aligned data (.p2align 7)                │       │
│  │  • Branchless CSEL/CSETM conditional operations                 │       │
│  │  • TBNZ single-bit branch tests for error checks                │       │
│  │  • 8KB buffer reads for memory bandwidth utilization            │       │
│  │  • TLS keep-alive + DNS cache (avoid reconnect)                 │       │
│  └─────────────────────────────────────────────────────────────────┘       │
│                                                                              │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Optimization Details

### 1. NEON SIMD (Advanced SIMD - 128-bit vectors)

Process 16 bytes per cycle using vector registers v0-v31:

```asm
// SIMD strlen - 16 bytes per iteration
strlen_simd:
    movi    v1.16b, #0              // Zero vector
.loop:
    ldr     q0, [x0]                // Load 16 bytes
    prfm    pldl1strm, [x0, #256]   // Prefetch ahead
    cmeq    v0.16b, v0.16b, v1.16b  // Compare each byte with 0
    umaxv   b2, v0.16b              // Horizontal max (non-zero if null found)
    umov    w11, v2.b[0]            // Extract low byte safely
    cbnz    w11, .found
    add     x0, x0, #16
    b       .loop
```

| Function | SIMD Technique | Speedup vs scalar |
|----------|----------------|-------------------|
| `strlen_simd` | `cmeq` + `umaxv` horizontal reduction | ~8-16x |
| `strcpy_simd` | 128-bit `ldr q` / `str q` | ~8-16x |
| Memory zero | `stp q0, q0` (32 bytes/instruction) | ~8x |
| Pattern match | `fmov x, d` for 8-byte compare | ~4x |

### 2. Cache Optimization

Apple Silicon performance cores have large L1 caches and strong hardware prefetch.
We align data on cache-line boundaries and use PRFM hints to keep hot streams in L1.

```asm
// Data alignment for cache efficiency
.p2align 7                          // 128-byte = cache line
buffer: .skip 8192

// Prefetch before use
prfm    pldl1strm, [x0, #256]       // Load prefetch, streaming
prfm    pstl1strm, [x1]             // Store prefetch, streaming
```

| Technique | Implementation |
|-----------|----------------|
| Data alignment | `.p2align 7` (128-byte cache lines) |
| Code alignment | `.p2align 4` (16-byte for fetch unit) |
| Read prefetch | `prfm pldl1strm, [addr, #256]` |
| Write prefetch | `prfm pstl1strm, [addr]` |
| Buffer size | 8KB reads for TLP efficiency |

### 3. Branchless Programming

Eliminate branch misprediction penalties using conditional operations:

```asm
// Instead of: if (x0 < 0) goto error
// Use bit test on sign bit:
tbnz    x0, #63, error      // Test bit 63 directly

// Instead of: x3 = (x0 < 0) ? 0 : x0
csel    x3, xzr, x0, lt     // Conditional select

// Instead of: x0 = (condition) ? -1 : 0  
csetm   x0, lt              // Conditional set mask
```

### 4. ARMv8+ Instructions Used

| Feature | Usage in nanocode |
|---------|-------------------|
| Fused compare-branch | `cbz`, `cbnz`, `tbnz` |
| Load/store pair | `ldp`, `stp` for 16-byte ops |
| Conditional select | `csel`, `csetm` for branchless ops |
| Prefetch hints | `prfm` load/store streaming |

### 5. Memory Access Patterns

```asm
// Sequential access enables hardware prefetcher
.Lread_loop:
    add     x1, x9, x19         // Sequential offset
    bl      _SSLRead
    add     x19, x19, x1        // Advance pointer
    
    // Manual prefetch for next iteration
    add     x10, x9, x19
    prfm    pstl1strm, [x10]
    prfm    pstl1strm, [x10, #64]
```

### 6. Connection Reuse + HTTP Parsing

| Optimization | Benefit |
|--------------|---------|
| TLS keep-alive | Avoid full handshake on each prompt |
| DNS cache | `getaddrinfo` once, reuse `addrinfo` |
| Content-Length parse | Stop reads exactly at body end |
| JSON escape fast-path | SIMD copy if no escapes needed |

```
Response read flow:
1) Read until \\r\\n\\r\\n header end found
2) Parse Content-Length
3) Read exactly body length
```

---

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     nanocode (assembly)                      │
├─────────────────────────────────────────────────────────────┤
│  User I/O        │  syscall read/write (SIMD strlen)        │
│  DNS Resolution  │  getaddrinfo() cached                    │
│  TCP Socket      │  syscall socket/connect                   │
│  TLS Layer       │  Security.framework (keep-alive)         │
│  HTTP/1.1        │  SIMD build + Content-Length parse       │
│  JSON Parse      │  NEON pattern match + escape fast-path   │
└─────────────────────────────────────────────────────────────┘
```

---

## Metrics

| Metric | Value |
|--------|-------|
| Source lines | ~1310 |
| Binary size | 54 KB |
| Text segment | 16 KB |
| SIMD functions | strlen, strcpy, memzero, pattern |
| Prefetch hints | 12+ PRFM instructions |
| Branchless ops | CSEL, CSETM, TBNZ |

---

## What About GPU / Neural Engine / AMX?

| Accelerator | Used? | Why |
|-------------|-------|-----|
| **GPU** (Metal) | ❌ | HTTP client is I/O bound, not compute bound |
| **ANE** (Neural Engine) | ❌ | Only accessible via CoreML, not assembly |
| **AMX** (Matrix coprocessor) | ❌ | Undocumented, only via Accelerate.framework |

These accelerators excel at:
- **GPU**: Parallel graphics/compute shaders
- **ANE**: Neural network inference (int8/fp16 matrix ops)
- **AMX**: Large matrix multiplications

For an HTTP client, the bottleneck is **network latency** (~50-200ms), not CPU compute. NEON SIMD is sufficient for string processing which takes microseconds.

---

## Pseudo-code Flow

```
PROGRAM nanocode:
    PRINT "nanocode | M5 Pro Max Native"
    
    LOOP:
        PRINT "❯ "
        input = READ_LINE()      // uses gets_fast with prefetch
        
        IF input is empty: CONTINUE
        IF input == "/q": EXIT
        IF input == "/c": PRINT "⏺ Cleared"; CONTINUE
        
        // Build request (fast-path if no JSON escapes)
        if not api_key_cached: api_key = getenv("ANTHROPIC_API_KEY")
        body = body1 + escape_if_needed(input) + body2
        body_len = strlen_simd(body)
        request = build_http_headers() + itoa_fast(body_len) + body
        
        // DNS + Connect (cached)
        if not addr_cached: addr = getaddrinfo("api.anthropic.com", "443")
        if not conn_ready:
            sock = socket(addr.family, addr.socktype, addr.protocol)
            connect(sock, addr)
            tls_handshake(sock)
        
        // TLS (Security.framework)
        ssl = cached_ctx_or_new()
        
        // Send/Receive with prefetch
        SSLWrite(ssl, request, len)
        response = SSLRead_until_header()
        content_len = parse_Content_Length(response)
        body = SSLRead_exact(content_len)
        
        // Extract response using NEON pattern match
        text = find_text_simd(response)  // finds "text":" pattern
        PRINT "⏺ " + text
        
        // Cleanup
        keep_alive(ssl, sock, addr)
```

---

## Quick Reference: Key NEON Instructions

| Instruction | Description | Used For |
|-------------|-------------|----------|
| `ldr q0, [x0]` | Load 128 bits | Fast string load |
| `str q0, [x0]` | Store 128 bits | Fast string copy |
| `movi v0.16b, #0` | Set all bytes to 0 | Zero vector for compare |
| `cmeq v0.16b, v0.16b, v1.16b` | Compare equal (per byte) | Find null bytes |
| `umaxv b0, v0.16b` | Horizontal max | Check if any byte matched |
| `shrn v0.8b, v0.8h, #4` | Narrow with shift | Extract match positions |
| `fmov x0, d0` | Move NEON to GPR | Transfer match bits |
| `stp q0, q0, [x0]` | Store pair (32 bytes) | Fast memzero |

---

## License

MIT
