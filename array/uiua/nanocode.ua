# nanocode - minimal claude code alternative (Uiua)
# uiua run nanocode.ua
# Uiua: A stack-based array language (2023+)

# ANSI colors
R ← "\e[0m"
B ← "\e[1m"
D ← "\e[2m"
C ← "\e[36m"
G ← "\e[32m"
BL ← "\e[34m"

# Tool execution using stack operations
Read ← (
  &fras           # Read file
  ⊜□ ≠@\n .      # Split by newlines
  ≡(⊂ "| " ⊂⋕⊙.) # Add line numbers
  ⊃⊢/⊂            # Join with newlines
)

Write ← (
  &fwa            # Write file
  "ok"            # Return ok
)

Bash ← (
  &rs "sh" "-c"  # Run shell command
)

# Tool dispatcher
Tool ← (
  ⊙⊙⊙⊙≡°□        # Unbox arguments
  ⊗ {"read" "write" "bash" "glob" "grep"}  # Find tool index
  (Read|Write|Bash|"files..."|"matches...") # Dispatch
)

# Print header
&p ⊂ B ⊂ "nanocode" ⊂ R ⊂ " | " ⊂ D ⊂ "Uiua 🐚 Stack Magic" R
&p ""

# Schema
Schema ← {
  "read": {"description": "Read file", "required": ["path"]}
  "write": {"description": "Write file", "required": ["path", "content"]}
  "bash": {"description": "Run command", "required": ["cmd"]}
}

# Main loop
Messages ← []

Loop ← (
  &p ⊂ B ⊂ BL ⊂ "❯" ⊂ R " "
  &sc                    # Read input
  
  # Handle commands
  ⊗ {"" "/q" "/c"} .    # Check for special commands
  (
    ◌ Loop             # Empty: continue
  | ◌ "Goodbye!"       # /q: quit
  | ◌ &p ⊂ G ⊂ "⏺ Cleared" R
    [] Loop            # /c: clear and continue
  | # Normal input
    &p ""
    &p ⊂ C ⊂ "⏺" ⊂ R " Uiua speaks in stacks! ⊂⊃⊢/⊂"
    &p ⊂ D ⊂ "  Where tacit meets AI!" R
    &p ""
    
    # Would add to messages and call API
    Loop
  )
)

Loop

# The Uiua agentic loop:
# ⍥(Tool ⊃⊢/⊂)
# 
# Just 9 glyphs! Stack-based languages are incredibly
# concise for expressing computation flows.
#
# In the AI era, Uiua's visual operators could map
# directly to attention patterns:
#   ⊃ = fork (parallel processing)
#   ⊂ = join (concatenation)
#   ≡ = rows (batched operations)
