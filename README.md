# 🧬 Flavored Nanocode

**Single-file minimal Claude Code alternatives in 120+ languages and flavors.**

Each implementation is a complete agentic coding assistant with tool use — `read`, `write`, `edit`, `glob`, `grep`, `shell` — all in one file with minimal dependencies.

![screenshot](screenshot.png)

## 🗺️ All Flavors (120 implementations!)

### 🐍 Python
| Flavor | File | Deps |
|--------|------|------|
| vanilla | [`python/nanocode.py`](python/nanocode.py) | 0 |
| Flask | [`python/flask/nanocode.py`](python/flask/nanocode.py) | flask |
| FastAPI | [`python/fastapi/nanocode.py`](python/fastapi/nanocode.py) | fastapi |

### 📘 TypeScript
| Flavor | File | Deps |
|--------|------|------|
| Node | [`typescript/node/nanocode.ts`](typescript/node/nanocode.ts) | 0 |
| Bun | [`typescript/bun/nanocode.ts`](typescript/bun/nanocode.ts) | 0 |
| Deno | [`typescript/deno/nanocode.ts`](typescript/deno/nanocode.ts) | 0 |
| AI SDK | [`typescript/aisdk/nanocode.ts`](typescript/aisdk/nanocode.ts) | @ai-sdk |
| Hono | [`typescript/hono/nanocode.ts`](typescript/hono/nanocode.ts) | hono |
| Elysia | [`typescript/elysia/nanocode.ts`](typescript/elysia/nanocode.ts) | elysia |
| Effect | [`typescript/effect/nanocode.ts`](typescript/effect/nanocode.ts) | effect |

### 📒 JavaScript
| Flavor | File | Deps |
|--------|------|------|
| Node | [`javascript/node/nanocode.mjs`](javascript/node/nanocode.mjs) | 0 |
| Browser | [`javascript/browser/nanocode.html`](javascript/browser/nanocode.html) | 0 |
| CoffeeScript | [`coffeescript/nanocode.coffee`](coffeescript/nanocode.coffee) | 0 |

### 🦀 Rust
| Flavor | File | Deps |
|--------|------|------|
| vanilla | [`rust/nanocode.rs`](rust/nanocode.rs) | ureq |
| Axum | [`rust/axum/nanocode.rs`](rust/axum/nanocode.rs) | axum |

### 🐹 Go
| Flavor | File | Deps |
|--------|------|------|
| vanilla | [`go/nanocode.go`](go/nanocode.go) | 0 |
| Fiber | [`go/fiber/nanocode.go`](go/fiber/nanocode.go) | fiber |

### 🐚 Shell
| Flavor | File | Deps |
|--------|------|------|
| Bash | [`bash/nanocode.sh`](bash/nanocode.sh) | jq |
| Zsh | [`zsh/nanocode.zsh`](zsh/nanocode.zsh) | jq |
| just-curl | [`shell/nanocode.sh`](shell/nanocode.sh) | jq, curl |
| Fish | [`fish/nanocode.fish`](fish/nanocode.fish) | jq |
| Nushell | [`nushell/nanocode.nu`](nushell/nanocode.nu) | 0 |
| PowerShell | [`powershell/nanocode.ps1`](powershell/nanocode.ps1) | 0 |
| AWK | [`awk/nanocode.awk`](awk/nanocode.awk) | curl, jq |

### ☕ JVM Languages
| Language | File | Deps |
|----------|------|------|
| Java | [`java/Nanocode.java`](java/Nanocode.java) | 0 (Java 17+) |
| Kotlin | [`kotlin/nanocode.kt`](kotlin/nanocode.kt) | gson |
| Scala | [`scala/nanocode.scala`](scala/nanocode.scala) | requests |
| Groovy | [`groovy/nanocode.groovy`](groovy/nanocode.groovy) | gson |
| Clojure | [`clojure/nanocode.clj`](clojure/nanocode.clj) | babashka |

### 🔮 Functional Languages
| Language | File | Deps |
|----------|------|------|
| Haskell | [`haskell/nanocode.hs`](haskell/nanocode.hs) | aeson, http-conduit |
| OCaml | [`ocaml/nanocode.ml`](ocaml/nanocode.ml) | yojson, cohttp |
| F# | [`fsharp/nanocode.fsx`](fsharp/nanocode.fsx) | FSharp.Data |
| Elixir | [`elixir/nanocode.exs`](elixir/nanocode.exs) | req |
| Erlang | [`erlang/nanocode.erl`](erlang/nanocode.erl) | jsx |
| Gleam | [`gleam/nanocode.gleam`](gleam/nanocode.gleam) | gleam_http |
| PureScript | [`purescript/nanocode.purs`](purescript/nanocode.purs) | affjax |
| Elm | [`elm/nanocode.elm`](elm/nanocode.elm) | 0 (browser) |
| Koka | [`koka/nanocode.kk`](koka/nanocode.kk) | 0 |

### 💎 Scripting Languages
| Language | File | Deps |
|----------|------|------|
| Ruby | [`ruby/nanocode.rb`](ruby/nanocode.rb) | 0 |
| PHP | [`php/nanocode.php`](php/nanocode.php) | 0 |
| Perl | [`perl/nanocode.pl`](perl/nanocode.pl) | LWP, JSON::PP |
| Lua | [`lua/nanocode.lua`](lua/nanocode.lua) | cjson, socket |
| Luau | [`luau/nanocode.luau`](luau/nanocode.luau) | lune |
| Tcl | [`tcl/nanocode.tcl`](tcl/nanocode.tcl) | tls, json |

### ⚡ Systems Languages
| Language | File | Deps |
|----------|------|------|
| C | [`c/nanocode.c`](c/nanocode.c) | libcurl |
| C++ | [`cpp/nanocode.cpp`](cpp/nanocode.cpp) | libcurl |
| Zig | [`zig/nanocode.zig`](zig/nanocode.zig) | 0 |
| Swift | [`swift/nanocode.swift`](swift/nanocode.swift) | 0 |
| C# | [`csharp/Nanocode.cs`](csharp/Nanocode.cs) | 0 (.NET 6+) |
| Odin | [`odin/nanocode.odin`](odin/nanocode.odin) | 0 |
| Hare | [`hare/nanocode.ha`](hare/nanocode.ha) | 0 |

### 🏛️ Classic/Old Languages
| Language | Year | File | Notes |
|----------|------|------|-------|
| FORTRAN | 1957 | [`fortran/nanocode.f90`](fortran/nanocode.f90) | First high-level language |
| COBOL | 1959 | [`cobol/nanocode.cob`](cobol/nanocode.cob) | Still runs banks! |
| ALGOL | 1960 | [`algol/nanocode.alg`](algol/nanocode.alg) | Ancestor of most languages |
| BASIC | 1964 | [`basic/nanocode.bas`](basic/nanocode.bas) | Taught a generation |
| Simula | 1967 | [`simula/nanocode.sim`](simula/nanocode.sim) | First OOP language! |
| Pascal | 1970 | [`pascal/nanocode.pas`](pascal/nanocode.pas) | Structured programming |
| Smalltalk | 1972 | [`smalltalk/nanocode.st`](smalltalk/nanocode.st) | Pure OOP |
| Modula-2 | 1978 | [`modula2/nanocode.mod`](modula2/nanocode.mod) | Modules done right |

### 🖥️ Visual Basic & BASIC Family
| Language | File | Notes |
|----------|------|-------|
| Visual Basic | [`vb/nanocode.vb`](vb/nanocode.vb) | VB.NET |
| BASIC | [`basic/nanocode.bas`](basic/nanocode.bas) | Classic line numbers! |

### 🎮 GPU Shaders
| Language | File | Platform |
|----------|------|----------|
| GLSL | [`glsl/nanocode.glsl`](glsl/nanocode.glsl) | OpenGL/Vulkan |
| HLSL | [`hlsl/nanocode.hlsl`](hlsl/nanocode.hlsl) | DirectX |
| CUDA | [`cuda/nanocode.cu`](cuda/nanocode.cu) | NVIDIA |
| Metal | [`metal/nanocode.metal`](metal/nanocode.metal) | Apple |
| WGSL | [`wgsl/nanocode.wgsl`](wgsl/nanocode.wgsl) | WebGPU |

### 💻 Assembly
| Architecture | File | Notes |
|--------------|------|-------|
| x86-64 | [`assembly/x86/nanocode.asm`](assembly/x86/nanocode.asm) | Linux |
| x86-64 SIMD | [`assembly/simd/nanocode.asm`](assembly/simd/nanocode.asm) | AVX2/SSE4.2! |
| ARM64 | [`assembly/arm/nanocode.s`](assembly/arm/nanocode.s) | aarch64 |
| RISC-V | [`assembly/riscv/nanocode.s`](assembly/riscv/nanocode.s) | rv64 |
| WebAssembly | [`wasm/nanocode.wat`](wasm/nanocode.wat) | WAT format |

### 📖 Literate Programming
| Format | File | Notes |
|--------|------|-------|
| Literate Haskell | [`literate/nanocode.lhs`](literate/nanocode.lhs) | Code IS documentation |
| Org-mode | [`org/nanocode.org`](org/nanocode.org) | Emacs literate |
| CWEB | [`cweb/nanocode.w`](cweb/nanocode.w) | Knuth's system |
| Prose | [`prose/nanocode.md`](prose/nanocode.md) | Natural language |

### 🚀 Modern Languages (post-2010)
| Language | Year | File | Deps |
|----------|------|------|------|
| Rust | 2010 | [`rust/nanocode.rs`](rust/nanocode.rs) | ureq |
| Kotlin | 2011 | [`kotlin/nanocode.kt`](kotlin/nanocode.kt) | gson |
| Elixir | 2011 | [`elixir/nanocode.exs`](elixir/nanocode.exs) | req |
| Julia | 2012 | [`julia/nanocode.jl`](julia/nanocode.jl) | HTTP, JSON3 |
| TypeScript | 2012 | [`typescript/node/nanocode.ts`](typescript/node/nanocode.ts) | 0 |
| Swift | 2014 | [`swift/nanocode.swift`](swift/nanocode.swift) | 0 |
| Crystal | 2014 | [`crystal/nanocode.cr`](crystal/nanocode.cr) | 0 |
| Nim | 2008/2019 | [`nim/nanocode.nim`](nim/nanocode.nim) | 0 |
| Zig | 2016 | [`zig/nanocode.zig`](zig/nanocode.zig) | 0 |
| V | 2019 | [`v/nanocode.v`](v/nanocode.v) | 0 |
| Gleam | 2016 | [`gleam/nanocode.gleam`](gleam/nanocode.gleam) | gleam_http |
| Roc | 2019 | [`roc/nanocode.roc`](roc/nanocode.roc) | basic-cli |
| Unison | 2019 | [`unison/nanocode.u`](unison/nanocode.u) | 0 |
| Vale | 2019 | [`vale/nanocode.vale`](vale/nanocode.vale) | 0 |

### 🔥 Languages 2022+
| Language | Year | File | Notes |
|----------|------|------|-------|
| Mojo | 2023 | [`mojo/nanocode.mojo`](mojo/nanocode.mojo) | 🔥 Python superset for AI |
| MoonBit | 2023 | [`moonbit/nanocode.mbt`](moonbit/nanocode.mbt) | Cloud/edge computing |
| Bend | 2024 | [`bend/nanocode.bend`](bend/nanocode.bend) | Massively parallel! |
| Carbon | 2022 | [`carbon/nanocode.carbon`](carbon/nanocode.carbon) | Google's C++ successor |
| Uiua | 2023 | [`uiua/nanocode.ua`](uiua/nanocode.ua) | Stack-based array lang |

### 🧮 Proof Assistants & Verification
| Language | File | Purpose |
|----------|------|---------|
| Lean 4 | [`lean/nanocode.lean`](lean/nanocode.lean) | Theorem prover + prog lang |
| Coq | [`coq/nanocode.v`](coq/nanocode.v) | Proof assistant |
| Agda | [`agda/nanocode.agda`](agda/nanocode.agda) | Dependent types |
| Idris 2 | [`idris/nanocode.idr`](idris/nanocode.idr) | Practical dependent types |
| F* | [`fstar/nanocode.fst`](fstar/nanocode.fst) | Proof-oriented lang |
| Dafny | [`dafny/nanocode.dfy`](dafny/nanocode.dfy) | Verification-aware |
| TLA+ | [`tlaplus/nanocode.tla`](tlaplus/nanocode.tla) | Specification lang |

### 🔢 Array Languages
| Language | File | Notes |
|----------|------|-------|
| APL | [`apl/nanocode.apl`](apl/nanocode.apl) | The original ⍝ |
| BQN | [`bqn/nanocode.bqn`](bqn/nanocode.bqn) | Modern APL |
| Uiua | [`uiua/nanocode.ua`](uiua/nanocode.ua) | Stack + arrays |

### 📚 Stack Languages
| Language | File | Notes |
|----------|------|-------|
| Forth | [`forth/nanocode.fth`](forth/nanocode.fth) | The original (1970) |
| Factor | [`factor/nanocode.factor`](factor/nanocode.factor) | Modern concatenative |

### 🎭 Lisp Family
| Language | File | Deps |
|----------|------|------|
| Racket | [`racket/nanocode.rkt`](racket/nanocode.rkt) | http-easy |
| Common Lisp | [`lisp/nanocode.lisp`](lisp/nanocode.lisp) | dexador |
| Scheme | [`scheme/nanocode.scm`](scheme/nanocode.scm) | guile |
| Clojure | [`clojure/nanocode.clj`](clojure/nanocode.clj) | babashka |

### 🤖 AI/ML Era Languages
| Language | File | Purpose |
|----------|------|---------|
| Mojo 🔥 | [`mojo/nanocode.mojo`](mojo/nanocode.mojo) | Python superset for ML |
| Triton | [`triton/nanocode.py`](triton/nanocode.py) | GPU programming |
| Bend | [`bend/nanocode.bend`](bend/nanocode.bend) | Auto-parallel execution |
| CUDA | [`cuda/nanocode.cu`](cuda/nanocode.cu) | NVIDIA GPU |

### 🎪 Esoteric & Funny Languages
| Language | File | Notes |
|----------|------|-------|
| LOLCODE | [`lolcode/nanocode.lol`](lolcode/nanocode.lol) | HAI! CAN HAS CHEEZBURGER? |
| Brainfuck | [`brainfuck/nanocode.bf`](brainfuck/nanocode.bf) | ++++++++++[>...] |
| Rockstar | [`rockstar/nanocode.rock`](rockstar/nanocode.rock) | 🎸 Code like lyrics |
| Shakespeare | [`shakespeare/nanocode.spl`](shakespeare/nanocode.spl) | Forsooth, thy code! |
| Chef | [`chef/nanocode.chef`](chef/nanocode.chef) | Recipe programming |
| Emojicode | [`emojicode/nanocode.emojic`](emojicode/nanocode.emojic) | 🦄 Programming |
| ArnoldC | [`arnoldc/nanocode.arnoldc`](arnoldc/nanocode.arnoldc) | I'LL BE BACK |
| Whitespace | [`whitespace/nanocode.ws`](whitespace/nanocode.ws) | Only ␣ ⇥ ↵ |
| Befunge | [`befunge/nanocode.bf93`](befunge/nanocode.bf93) | 2D code flow |
| INTERCAL | [`intercal/nanocode.i`](intercal/nanocode.i) | PLEASE DO NOT |
| Malbolge | [`malbolge/nanocode.mal`](malbolge/nanocode.mal) | Impossible |

### 📝 Other Notable
| Language | File | Notes |
|----------|------|-------|
| Prolog | [`prolog/nanocode.pl`](prolog/nanocode.pl) | Logic programming |
| Ada | [`ada/nanocode.adb`](ada/nanocode.adb) | Safety-critical |
| Pony | [`pony/nanocode.pony`](pony/nanocode.pony) | Actor model |
| Chapel | [`chapel/nanocode.chpl`](chapel/nanocode.chpl) | Parallel computing |
| D | [`d/nanocode.d`](d/nanocode.d) | Better C++ |
| Dart | [`dart/nanocode.dart`](dart/nanocode.dart) | Flutter's language |
| R | [`r/nanocode.R`](r/nanocode.R) | Statistics |

## 🚀 Quick Start

```bash
# Set your API key
export ANTHROPIC_API_KEY="sk-..."

# Or use OpenRouter for any model
export OPENROUTER_API_KEY="sk-..."
export MODEL="anthropic/claude-opus-4"

# Pick your flavor and run!
python python/nanocode.py
bun typescript/bun/nanocode.ts
deno run -A typescript/deno/nanocode.ts
./bash/nanocode.sh
ruby ruby/nanocode.rb
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
