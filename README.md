# 🧬 Flavored Nanocode

**Single-file minimal Claude Code alternatives in 120+ languages and flavors.**

Each implementation is a complete agentic coding assistant with tool use — `read`, `write`, `edit`, `glob`, `grep`, `shell` — all in one file with minimal dependencies.

![screenshot](screenshot.png)

## 🗺️ All Flavors (120 implementations!)

### 🐍 Python
| Flavor | File | Deps |
|--------|------|------|
| vanilla | [`interpreted/python/nanocode.py`](interpreted/python/nanocode.py) | 0 |
| Flask | [`interpreted/python/flask/nanocode.py`](interpreted/python/flask/nanocode.py) | flask |
| FastAPI | [`interpreted/python/fastapi/nanocode.py`](interpreted/python/fastapi/nanocode.py) | fastapi |

### 📘 TypeScript
| Flavor | File | Deps |
|--------|------|------|
| Node | [`interpreted/typescript/node/nanocode.ts`](interpreted/typescript/node/nanocode.ts) | 0 |
| Bun | [`interpreted/typescript/bun/nanocode.ts`](interpreted/typescript/bun/nanocode.ts) | 0 |
| Deno | [`interpreted/typescript/deno/nanocode.ts`](interpreted/typescript/deno/nanocode.ts) | 0 |
| AI SDK | [`interpreted/typescript/aisdk/nanocode.ts`](interpreted/typescript/aisdk/nanocode.ts) | @ai-sdk |
| Hono | [`interpreted/typescript/hono/nanocode.ts`](interpreted/typescript/hono/nanocode.ts) | hono |
| Elysia | [`interpreted/typescript/elysia/nanocode.ts`](interpreted/typescript/elysia/nanocode.ts) | elysia |
| Effect | [`interpreted/typescript/effect/nanocode.ts`](interpreted/typescript/effect/nanocode.ts) | effect |

### 📒 JavaScript
| Flavor | File | Deps |
|--------|------|------|
| Node | [`interpreted/javascript/node/nanocode.mjs`](interpreted/javascript/node/nanocode.mjs) | 0 |
| Browser | [`interpreted/javascript/browser/nanocode.html`](interpreted/javascript/browser/nanocode.html) | 0 |
| CoffeeScript | [`interpreted/coffeescript/nanocode.coffee`](interpreted/coffeescript/nanocode.coffee) | 0 |

### 🦀 Rust
| Flavor | File | Deps |
|--------|------|------|
| vanilla | [`compiled/rust/nanocode.rs`](compiled/rust/nanocode.rs) | ureq |
| Axum | [`compiled/rust/axum/nanocode.rs`](compiled/rust/axum/nanocode.rs) | axum |

### 🐹 Go
| Flavor | File | Deps |
|--------|------|------|
| vanilla | [`compiled/go/nanocode.go`](compiled/go/nanocode.go) | 0 |
| Fiber | [`compiled/go/fiber/nanocode.go`](compiled/go/fiber/nanocode.go) | fiber |

### 🐚 Shell
| Flavor | File | Deps |
|--------|------|------|
| Bash | [`shell/bash/nanocode.sh`](shell/bash/nanocode.sh) | jq |
| Zsh | [`shell/zsh/nanocode.zsh`](shell/zsh/nanocode.zsh) | jq |
| just-curl | [`shell/nanocode.sh`](shell/nanocode.sh) | jq, curl |
| Fish | [`shell/fish/nanocode.fish`](shell/fish/nanocode.fish) | jq |
| Nushell | [`shell/nushell/nanocode.nu`](shell/nushell/nanocode.nu) | 0 |
| PowerShell | [`shell/powershell/nanocode.ps1`](shell/powershell/nanocode.ps1) | 0 |
| AWK | [`interpreted/awk/nanocode.awk`](interpreted/awk/nanocode.awk) | curl, jq |

### ☕ JVM Languages
| Language | File | Deps |
|----------|------|------|
| Java | [`jvm/java/Nanocode.java`](jvm/java/Nanocode.java) | 0 (Java 17+) |
| Kotlin | [`jvm/kotlin/nanocode.kt`](jvm/kotlin/nanocode.kt) | gson |
| Scala | [`functional/scala/nanocode.scala`](functional/scala/nanocode.scala) | requests |
| Groovy | [`interpreted/groovy/nanocode.groovy`](interpreted/groovy/nanocode.groovy) | gson |
| Clojure | [`functional/clojure/nanocode.clj`](functional/clojure/nanocode.clj) | babashka |

### 🔮 Functional Languages
| Language | File | Deps |
|----------|------|------|
| Haskell | [`functional/haskell/nanocode.hs`](functional/haskell/nanocode.hs) | aeson, http-conduit |
| OCaml | [`functional/ocaml/nanocode.ml`](functional/ocaml/nanocode.ml) | yojson, cohttp |
| F# | [`functional/fsharp/nanocode.fsx`](functional/fsharp/nanocode.fsx) | FSharp.Data |
| Elixir | [`functional/elixir/nanocode.exs`](functional/elixir/nanocode.exs) | req |
| Erlang | [`functional/erlang/nanocode.erl`](functional/erlang/nanocode.erl) | jsx |
| Gleam | [`functional/gleam/nanocode.gleam`](functional/gleam/nanocode.gleam) | gleam_http |
| PureScript | [`functional/purescript/nanocode.purs`](functional/purescript/nanocode.purs) | affjax |
| Elm | [`functional/elm/nanocode.elm`](functional/elm/nanocode.elm) | 0 (browser) |
| Koka | [`functional/koka/nanocode.kk`](functional/koka/nanocode.kk) | 0 |

### 💎 Scripting Languages
| Language | File | Deps |
|----------|------|------|
| Ruby | [`interpreted/ruby/nanocode.rb`](interpreted/ruby/nanocode.rb) | 0 |
| PHP | [`interpreted/php/nanocode.php`](interpreted/php/nanocode.php) | 0 |
| Perl | [`interpreted/perl/nanocode.pl`](interpreted/perl/nanocode.pl) | LWP, JSON::PP |
| Lua | [`interpreted/lua/nanocode.lua`](interpreted/lua/nanocode.lua) | cjson, socket |
| Luau | [`interpreted/luau/nanocode.luau`](interpreted/luau/nanocode.luau) | lune |
| Tcl | [`interpreted/tcl/nanocode.tcl`](interpreted/tcl/nanocode.tcl) | tls, json |

### ⚡ Systems Languages
| Language | File | Deps |
|----------|------|------|
| C | [`compiled/c/nanocode.c`](compiled/c/nanocode.c) | libcurl |
| C++ | [`compiled/cpp/nanocode.cpp`](compiled/cpp/nanocode.cpp) | libcurl |
| Zig | [`zig/nanocode.zig`](zig/nanocode.zig) | 0 |
| Zig (BLAS) | [`zig/blas/nanocode.zig`](zig/blas/nanocode.zig) | Accelerate (BLAS, macOS) |
| Swift | [`compiled/swift/nanocode.swift`](compiled/swift/nanocode.swift) | 0 |
| C# | [`dotnet/csharp/Nanocode.cs`](dotnet/csharp/Nanocode.cs) | 0 (.NET 6+) |
| Odin | [`compiled/odin/nanocode.odin`](compiled/odin/nanocode.odin) | 0 |
| Hare | [`compiled/hare/nanocode.ha`](compiled/hare/nanocode.ha) | 0 |

### 🏛️ Classic/Old Languages
| Language | Year | File | Notes |
|----------|------|------|-------|
| FORTRAN | 1957 | [`compiled/fortran/nanocode.f90`](compiled/fortran/nanocode.f90) | First high-level language |
| COBOL | 1959 | [`compiled/cobol/nanocode.cob`](compiled/cobol/nanocode.cob) | Still runs banks! |
| ALGOL | 1960 | [`compiled/algol/nanocode.alg`](compiled/algol/nanocode.alg) | Ancestor of most languages |
| BASIC | 1964 | [`interpreted/basic/nanocode.bas`](interpreted/basic/nanocode.bas) | Taught a generation |
| Simula | 1967 | [`oop/simula/nanocode.sim`](oop/simula/nanocode.sim) | First OOP language! |
| Pascal | 1970 | [`compiled/pascal/nanocode.pas`](compiled/pascal/nanocode.pas) | Structured programming |
| Smalltalk | 1972 | [`oop/smalltalk/nanocode.st`](oop/smalltalk/nanocode.st) | Pure OOP |
| Modula-2 | 1978 | [`compiled/modula2/nanocode.mod`](compiled/modula2/nanocode.mod) | Modules done right |

### 🖥️ Visual Basic & BASIC Family
| Language | File | Notes |
|----------|------|-------|
| Visual Basic | [`dotnet/vb/nanocode.vb`](dotnet/vb/nanocode.vb) | VB.NET |
| BASIC | [`interpreted/basic/nanocode.bas`](interpreted/basic/nanocode.bas) | Classic line numbers! |

### 🎮 GPU Shaders
| Language | File | Platform |
|----------|------|----------|
| GLSL | [`gpu/glsl/nanocode.glsl`](gpu/glsl/nanocode.glsl) | OpenGL/Vulkan |
| HLSL | [`gpu/hlsl/nanocode.hlsl`](gpu/hlsl/nanocode.hlsl) | DirectX |
| CUDA | [`gpu/cuda/nanocode.cu`](gpu/cuda/nanocode.cu) | NVIDIA |
| Metal | [`gpu/metal/nanocode.metal`](gpu/metal/nanocode.metal) | Apple |
| WGSL | [`gpu/wgsl/nanocode.wgsl`](gpu/wgsl/nanocode.wgsl) | WebGPU |

### 💻 Assembly
| Architecture | File | Notes |
|--------------|------|-------|
| x86-64 | [`systems/assembly/x86/nanocode.asm`](systems/assembly/x86/nanocode.asm) | Linux |
| x86-64 SIMD | [`systems/assembly/simd/nanocode.asm`](systems/assembly/simd/nanocode.asm) | AVX2/SSE4.2! |
| ARM64 | [`systems/assembly/arm/nanocode.s`](systems/assembly/arm/nanocode.s) | aarch64 |
| ARM64 (Apple Silicon + BLAS) | [`systems/assembly/arm/blas/nanocode.s`](systems/assembly/arm/blas/nanocode.s) | Accelerate (BLAS, macOS arm64) |
| RISC-V | [`systems/assembly/riscv/nanocode.s`](systems/assembly/riscv/nanocode.s) | rv64 |
| WebAssembly | [`systems/wasm/nanocode.wat`](systems/wasm/nanocode.wat) | WAT format |

### 📖 Literate Programming
| Format | File | Notes |
|--------|------|-------|
| Literate Haskell | [`markup/literate/nanocode.lhs`](markup/literate/nanocode.lhs) | Code IS documentation |
| Org-mode | [`markup/org/nanocode.org`](markup/org/nanocode.org) | Emacs literate |
| CWEB | [`markup/cweb/nanocode.w`](markup/cweb/nanocode.w) | Knuth's system |
| Prose | [`markup/prose/nanocode.prose`](markup/prose/nanocode.prose) | Natural language |

### 🚀 Modern Languages (post-2010)
| Language | Year | File | Deps |
|----------|------|------|------|
| Rust | 2010 | [`compiled/rust/nanocode.rs`](compiled/rust/nanocode.rs) | ureq |
| Kotlin | 2011 | [`jvm/kotlin/nanocode.kt`](jvm/kotlin/nanocode.kt) | gson |
| Elixir | 2011 | [`functional/elixir/nanocode.exs`](functional/elixir/nanocode.exs) | req |
| Julia | 2012 | [`interpreted/julia/nanocode.jl`](interpreted/julia/nanocode.jl) | HTTP, JSON3 |
| TypeScript | 2012 | [`interpreted/typescript/node/nanocode.ts`](interpreted/typescript/node/nanocode.ts) | 0 |
| Swift | 2014 | [`compiled/swift/nanocode.swift`](compiled/swift/nanocode.swift) | 0 |
| Crystal | 2014 | [`compiled/crystal/nanocode.cr`](compiled/crystal/nanocode.cr) | 0 |
| Nim | 2008/2019 | [`compiled/nim/nanocode.nim`](compiled/nim/nanocode.nim) | 0 |
| Zig | 2016 | [`zig/nanocode.zig`](zig/nanocode.zig) | 0 |
| V | 2019 | [`compiled/v/nanocode.v`](compiled/v/nanocode.v) | 0 |
| Gleam | 2016 | [`functional/gleam/nanocode.gleam`](functional/gleam/nanocode.gleam) | gleam_http |
| Roc | 2019 | [`functional/roc/nanocode.roc`](functional/roc/nanocode.roc) | basic-cli |
| Unison | 2019 | [`functional/unison/nanocode.u`](functional/unison/nanocode.u) | 0 |
| Vale | 2019 | [`compiled/vale/nanocode.vale`](compiled/vale/nanocode.vale) | 0 |

### 🔥 Languages 2022+
| Language | Year | File | Notes |
|----------|------|------|-------|
| Mojo | 2023 | [`compiled/mojo/nanocode.mojo`](compiled/mojo/nanocode.mojo) | 🔥 Python superset for AI |
| MoonBit | 2023 | [`compiled/moonbit/nanocode.mbt`](compiled/moonbit/nanocode.mbt) | Cloud/edge computing |
| Bend | 2024 | [`compiled/bend/nanocode.bend`](compiled/bend/nanocode.bend) | Massively parallel! |
| Carbon | 2022 | [`compiled/carbon/nanocode.carbon`](compiled/carbon/nanocode.carbon) | Google's C++ successor |
| Uiua | 2023 | [`array/uiua/nanocode.ua`](array/uiua/nanocode.ua) | Stack-based array lang |

### 🧮 Proof Assistants & Verification
| Language | File | Purpose |
|----------|------|---------|
| Lean 4 | [`proof/lean/nanocode.lean`](proof/lean/nanocode.lean) | Theorem prover + prog lang |
| Coq | [`proof/coq/nanocode.v`](proof/coq/nanocode.v) | Proof assistant |
| Agda | [`proof/agda/nanocode.agda`](proof/agda/nanocode.agda) | Dependent types |
| Idris 2 | [`proof/idris/nanocode.idr`](proof/idris/nanocode.idr) | Practical dependent types |
| F* | [`proof/fstar/nanocode.fst`](proof/fstar/nanocode.fst) | Proof-oriented lang |
| Dafny | [`proof/dafny/nanocode.dfy`](proof/dafny/nanocode.dfy) | Verification-aware |
| TLA+ | [`proof/tlaplus/nanocode.tla`](proof/tlaplus/nanocode.tla) | Specification lang |

### 🔢 Array Languages
| Language | File | Notes |
|----------|------|-------|
| APL | [`array/apl/nanocode.apl`](array/apl/nanocode.apl) | The original ⍝ |
| BQN | [`array/bqn/nanocode.bqn`](array/bqn/nanocode.bqn) | Modern APL |
| Uiua | [`array/uiua/nanocode.ua`](array/uiua/nanocode.ua) | Stack + arrays |

### 📚 Stack Languages
| Language | File | Notes |
|----------|------|-------|
| Forth | [`stack/forth/nanocode.fth`](stack/forth/nanocode.fth) | The original (1970) |
| Factor | [`stack/factor/nanocode.factor`](stack/factor/nanocode.factor) | Modern concatenative |

### 🎭 Lisp Family
| Language | File | Deps |
|----------|------|------|
| Racket | [`functional/racket/nanocode.rkt`](functional/racket/nanocode.rkt) | http-easy |
| Common Lisp | [`functional/lisp/nanocode.lisp`](functional/lisp/nanocode.lisp) | dexador |
| Scheme | [`functional/scheme/nanocode.scm`](functional/scheme/nanocode.scm) | guile |
| Clojure | [`functional/clojure/nanocode.clj`](functional/clojure/nanocode.clj) | babashka |

### 🤖 AI/ML Era Languages
| Language | File | Purpose |
|----------|------|---------|
| Mojo 🔥 | [`compiled/mojo/nanocode.mojo`](compiled/mojo/nanocode.mojo) | Python superset for ML |
| Triton | [`gpu/triton/nanocode.py`](gpu/triton/nanocode.py) | GPU programming |
| Bend | [`compiled/bend/nanocode.bend`](compiled/bend/nanocode.bend) | Auto-parallel execution |
| CUDA | [`gpu/cuda/nanocode.cu`](gpu/cuda/nanocode.cu) | NVIDIA GPU |

### 🎪 Esoteric & Funny Languages
| Language | File | Notes |
|----------|------|-------|
| LOLCODE | [`esoteric/lolcode/nanocode.lol`](esoteric/lolcode/nanocode.lol) | HAI! CAN HAS CHEEZBURGER? |
| Brainfuck | [`esoteric/brainfuck/nanocode.bf`](esoteric/brainfuck/nanocode.bf) | ++++++++++[>...] |
| Rockstar | [`esoteric/rockstar/nanocode.rock`](esoteric/rockstar/nanocode.rock) | 🎸 Code like lyrics |
| Shakespeare | [`esoteric/shakespeare/nanocode.spl`](esoteric/shakespeare/nanocode.spl) | Forsooth, thy code! |
| Chef | [`esoteric/chef/nanocode.chef`](esoteric/chef/nanocode.chef) | Recipe programming |
| Emojicode | [`esoteric/emojicode/nanocode.emojic`](esoteric/emojicode/nanocode.emojic) | 🦄 Programming |
| ArnoldC | [`esoteric/arnoldc/nanocode.arnoldc`](esoteric/arnoldc/nanocode.arnoldc) | I'LL BE BACK |
| Whitespace | [`esoteric/whitespace/nanocode.ws`](esoteric/whitespace/nanocode.ws) | Only ␣ ⇥ ↵ |
| Befunge | [`esoteric/befunge/nanocode.bf93`](esoteric/befunge/nanocode.bf93) | 2D code flow |
| INTERCAL | [`esoteric/intercal/nanocode.i`](esoteric/intercal/nanocode.i) | PLEASE DO NOT |
| Malbolge | [`esoteric/malbolge/nanocode.mal`](esoteric/malbolge/nanocode.mal) | Impossible |

### 📝 Other Notable
| Language | File | Notes |
|----------|------|-------|
| Prolog | [`logic/prolog/nanocode.pl`](logic/prolog/nanocode.pl) | Logic programming |
| Ada | [`compiled/ada/nanocode.adb`](compiled/ada/nanocode.adb) | Safety-critical |
| Pony | [`compiled/pony/nanocode.pony`](compiled/pony/nanocode.pony) | Actor model |
| Chapel | [`compiled/chapel/nanocode.chpl`](compiled/chapel/nanocode.chpl) | Parallel computing |
| D | [`compiled/d/nanocode.d`](compiled/d/nanocode.d) | Better C++ |
| Dart | [`interpreted/dart/nanocode.dart`](interpreted/dart/nanocode.dart) | Flutter's language |
| R | [`interpreted/r/nanocode.R`](interpreted/r/nanocode.R) | Statistics |

## 🚀 Quick Start

```bash
# Set your API key
export ANTHROPIC_API_KEY="sk-..."

# Or use OpenRouter for any model
export OPENROUTER_API_KEY="sk-..."
export MODEL="anthropic/claude-opus-4"

# Pick your flavor and run!
python interpreted/python/nanocode.py
bun interpreted/typescript/bun/nanocode.ts
deno run -A interpreted/typescript/deno/nanocode.ts
./shell/bash/nanocode.sh
ruby interpreted/ruby/nanocode.rb
```

## 🔧 Features

All implementations include:
- **Agentic loop** — continuous tool use until task complete
- **File ops** — `read`, `write`, `edit` with line numbers
- **Search** — `glob` (find files), `grep` (search content)
- **Shell** — execute commands with output
- **Colors** — pretty terminal output
- **History** — conversation context

## 📐 Philosophy

1. **Single file** — copy-paste friendly
2. **Minimal deps** — prefer stdlib
3. **~60-130 lines** — readable in one sitting
4. **Same tools** — consistent API across languages
5. **Zero config** — just API key and go

## 🎨 Commands

| Command | Action |
|---------|--------|
| `/c` | Clear conversation |
| `/q` | Quit |

## 🤯 Why So Many Languages?

In the AI era, we explore:
- **Old languages** → COBOL/FORTRAN still run the world
- **Proof assistants** → Verified AI agents
- **Array languages** → Batch operations like attention
- **GPU shaders** → Parallel tool execution
- **Literate programming** → Self-documenting agents
- **Esoteric languages** → Testing AI comprehension
- **New languages** → Future of AI-native coding
- **Assembly + SIMD** → Understanding what AI abstracts away

## 📝 Contributing

Add your favorite language! Requirements:
- Single file
- Under 150 lines (ideally ~60-80)
- Minimal dependencies  
- Core tools: read, write, edit, glob, grep, shell

## License

MIT
