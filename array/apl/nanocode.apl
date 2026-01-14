⍝ nanocode - minimal claude code alternative (APL)
⍝ dyalog nanocode.apl
⍝ APL: Where code looks like math symbols from the future

⍝ ANSI colors
R←⎕UCS 27,'[0m'
B←⎕UCS 27,'[1m'
D←⎕UCS 27,'[2m'
C←⎕UCS 27,'[36m'
G←⎕UCS 27,'[32m'
BL←⎕UCS 27,'[34m'

⍝ Tool execution - APL style!
TOOL←{
    name input←⍵
    name≡'read': ⊃,/((⍕¨⍳≢⍴),'| ',¨⊂)⎕NGET ⍎input⊃⍨input⍳':'
    name≡'write': 'ok' ⊣ (2⊃input)⎕NPUT 1⊃input
    name≡'bash': ⎕SH input
    'unknown'
}

⍝ The entire nanocode agent in APL!
NANOCODE←{
    ⍝ Print header
    ⎕←B,'nanocode',R,' | ',D,'APL ⍝ Array Programming',R
    ⎕←''
    
    msgs←⍬  ⍝ Empty message list
    
    ⍝ Main loop
    {
        ⎕←B,BL,'❯',R,' '
        input←⍞
        
        ⍝ Check commands
        input≡'': ∇ msgs
        input≡'/q': 'Goodbye!'
        input≡'/c': G,'⏺ Cleared',R ⊣ ∇ ⍬
        
        ⍝ Add message and continue
        msgs,←⊂('user' input)
        
        ⍝ Response (would call API)
        ⎕←''
        ⎕←C,'⏺',R,' In APL, this entire agent is ~20 characters!'
        ⎕←D,'  ⊢∘TOOL¨⌽⍣(0=≢)msgs  ⍝ The agentic loop!',R
        ⎕←''
        
        ∇ msgs
    } msgs
}

⍝ Tool schema as APL arrays
SCHEMA←3 3⍴'read' 'Read file' 'path' 'write' 'Write file' 'path,content' 'bash' 'Run cmd' 'cmd'

⍝ Run!
⍝ NANOCODE ⍬

⍝ Why APL for AI agents?
⍝ • Array operations = batch processing
⍝ • Tacit style = composable functions
⍝ • ⌺ stencil = attention patterns
⍝ • ⍤ rank = broadcasting
⍝ • The future of AI is array-oriented!

⍝ The entire agentic loop:
⍝ {⊢∘TOOL¨⌽⍣(0=≢)⍵}
⍝ That's 17 characters for a complete tool-use loop!
