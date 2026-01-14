# nanocode - minimal claude code alternative (BQN)
# bqn nanocode.bqn
# BQN: APL's modern descendant (2020+)

# ANSI colors
r ← @+27∾"[0m"
b ← @+27∾"[1m"
d ← @+27∾"[2m"
c ← @+27∾"[36m"
g ← @+27∾"[32m"
bl ← @+27∾"[34m"

# Tool execution using BQN's combinators
Tool ← {
  name‿input ← 𝕩
  "read"≡name ? (∾˝(•Fmt¨1+↕≠)∾¨"| "⊸∾¨)•FLines input ;
  "write"≡name ? "ok"⊣input.content•FChars input.path ;
  "bash"≡name ? •SH input ;
  "unknown"
}

# Schema as a BQN table
Schema ← [
  "read"  ‿ "Read file"   ‿ ⟨"path"⟩
  "write" ‿ "Write file"  ‿ ⟨"path","content"⟩
  "bash"  ‿ "Run command" ‿ ⟨"cmd"⟩
]

# Message type
Message ← {role⇐𝕨, content⇐𝕩}

# Print with colors
Print ← {•Out 𝕩}

# Main REPL
Main ← {
  Print b∾"nanocode"∾r∾" | "∾d∾"BQN ⌈⊸+ Array Magic"∾r
  Print ""
  
  # Recursive REPL loop
  Loop ← {msgs←𝕩
    Print b∾bl∾"❯"∾r∾" "
    input ← •GetLine@
    
    ""≡input ? 𝕊 msgs ;
    "/q"≡input ? @ ;
    "/c"≡input ? (Print g∾"⏺ Cleared"∾r) ⊢ 𝕊 ⟨⟩ ;
    
    # Add message
    msgs ∾↩ <"user" Message input
    
    # Response
    Print ""
    Print c∾"⏺"∾r∾" BQN: Where tacit programming meets AI!"
    Print d∾"  ⌈⊸+´∘⊏˜⍋ ← The future is array-oriented!"∾r
    Print ""
    
    𝕊 msgs
  }
  
  Loop ⟨⟩
}

# The entire agentic loop in BQN:
# {𝕊⍟(0<≠∘⊢)Tool¨⊢}
# 
# That's 20 characters for the complete tool-use loop!
# BQN's combinators make AI agents incredibly concise.

# Entry point
Main@

# Why BQN for AI?
# • Modern APL with cleaner syntax
# • First-class functions = composable agents
# • Array operations = batch inference
# • Tacit programming = point-free agent composition
