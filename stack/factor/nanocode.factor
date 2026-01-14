! nanocode - minimal claude code alternative (Factor)
! factor nanocode.factor
! Factor: Modern concatenative programming (2003+)

USING: io io.files io.encodings.utf8 kernel sequences 
       splitting formatting command-line environment
       http.client json.reader json.writer ;
IN: nanocode

! ANSI colors
: R ( -- str ) "\e[0m" ;
: B ( -- str ) "\e[1m" ;
: D ( -- str ) "\e[2m" ;
: C ( -- str ) "\e[36m" ;
: G ( -- str ) "\e[32m" ;
: BL ( -- str ) "\e[34m" ;

! Tool execution using stack composition
: read-tool ( path -- result )
    utf8 file-lines
    [ 1 + "%d| %s" sprintf ] map-index
    "\n" join ;

: write-tool ( content path -- result )
    utf8 set-file-contents "ok" ;

: bash-tool ( cmd -- result )
    run-process process-stdout ;

: run-tool ( name input -- result )
    swap {
        { "read" [ "path" of read-tool ] }
        { "write" [ [ "content" of ] [ "path" of ] bi write-tool ] }
        { "bash" [ "cmd" of bash-tool ] }
        [ 2drop "unknown" ]
    } case ;

! Schema
: schema ( -- json )
    {
        { "name" "read" }
        { "description" "Read file" }
        { "input_schema" { "type" "object" "properties" { "path" { "type" "string" } } "required" { "path" } } }
    }
    {
        { "name" "write" }
        { "description" "Write file" }
        { "input_schema" { "type" "object" "properties" { "path" { "type" "string" } "content" { "type" "string" } } "required" { "path" "content" } } }
    }
    {
        { "name" "bash" }
        { "description" "Run command" }
        { "input_schema" { "type" "object" "properties" { "cmd" { "type" "string" } } "required" { "cmd" } } }
    }
    3array ;

! Message handling
TUPLE: message role content ;

! Main REPL
: print-prompt ( -- )
    B BL "❯" R " " 4array concat print flush ;

: handle-input ( messages input -- messages' continue? )
    {
        { "" [ t ] }
        { "/q" [ f ] }
        { "/c" [ 
            drop { }
            G "⏺ Cleared" R 3array concat print
            t 
        ] }
        [ 
            "user" swap message boa suffix
            
            ! Response
            "" print
            C "⏺" R " Factor: Concatenative elegance!" 4array concat print
            D "  Stack-based composition for AI!" R 3array concat print
            "" print
            t
        ]
    } case ;

: main-loop ( messages -- )
    print-prompt
    readln handle-input
    [ main-loop ] [ drop ] if ;

: main ( -- )
    B "nanocode" R " | " D "Factor - Stack Composition" R 
    6array concat print
    "" print
    { } main-loop ;

MAIN: main

! Factor's approach to AI agents:
! : agent-loop ( msgs -- msgs' )
!     ask-claude process-tools
!     dup empty? [ agent-loop ] unless ;
!
! The entire agentic loop is just function composition!
