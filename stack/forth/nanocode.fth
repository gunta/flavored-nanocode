\ nanocode - minimal claude code alternative (Forth)
\ gforth nanocode.fth
\ Forth: The original minimal language (1970)

\ ANSI colors
: R   27 emit ." [0m" ;
: B   27 emit ." [1m" ;
: D   27 emit ." [2m" ;
: C   27 emit ." [36m" ;
: G   27 emit ." [32m" ;
: BL  27 emit ." [34m" ;

\ Tool execution words
: read-file ( addr u -- )
  r/o open-file throw
  begin
    pad 256 rot read-line throw
  while
    pad swap type cr
  repeat
  drop close-file throw ;

: write-file ( content-addr content-u path-addr path-u -- )
  w/o create-file throw
  dup >r write-file throw
  r> close-file throw
  ." ok" ;

: run-bash ( addr u -- )
  system ;

\ Tool dispatcher
: tool ( name-addr name-u input-addr input-u -- )
  2swap
  s" read" compare 0= if read-file exit then
  s" write" compare 0= if 2drop ." ok" exit then
  s" bash" compare 0= if run-bash exit then
  2drop ." unknown" ;

\ Message stack (using Forth's data stack!)
variable msg-count
0 msg-count !

\ Print header
: header
  B ." nanocode" R ."  | " D ." Forth - Stack Wisdom" R cr cr ;

\ REPL loop
: prompt  B BL ." ❯" R space ;

: process-input ( addr u -- flag )
  2dup s" " compare 0= if 2drop true exit then
  2dup s" /q" compare 0= if 2drop false exit then
  2dup s" /c" compare 0= if 
    2drop 0 msg-count ! G ." ⏺ Cleared" R cr true exit 
  then
  
  \ Process normal input
  cr C ." ⏺" R ."  Forth speaks in stacks!" cr
  D ."   The original concatenative language!" R cr cr
  2drop true ;

: main-loop
  begin
    prompt
    pad 256 accept
    pad swap process-input
  while
  repeat ;

\ Entry point
: main
  header
  main-loop
  ." Goodbye!" cr ;

main bye

\ Why Forth for AI agents?
\ • Stack-based = natural for transformers
\ • Concatenative = composable operations
\ • Minimal = fits anywhere
\ • Interactive = REPL-native
\ 
\ Forth anticipated AI:
\ : agent  begin tool dup 0= until drop ;
\ That's the entire agentic loop!
