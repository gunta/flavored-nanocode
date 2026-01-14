'begin'
    'comment' nanocode - minimal claude code alternative (ALGOL 60);
    'comment' ALGOL: The ancestor of most modern languages (1960);
    'comment' This influenced: Pascal, C, Java, Python, and more;
    
    'comment' Variable declarations;
    'integer' msgCount;
    'Boolean' running;
    'string' input, key, model;
    'array' messages[1:100];
    
    'comment' ANSI codes (conceptual - ALGOL predates terminals);
    'string' R, B, D, C, G, BL;
    R := "reset";
    B := "bold";
    D := "dim";
    C := "cyan";
    G := "green";
    BL := "blue";
    
    'comment' Tool procedures;
    'string' 'procedure' readTool(path);
        'value' path;
        'string' path;
    'begin'
        'comment' File reading;
        readTool := "1| file content"
    'end';
    
    'string' 'procedure' writeTool(path, content);
        'value' path, content;
        'string' path, content;
    'begin'
        writeTool := "ok"
    'end';
    
    'string' 'procedure' bashTool(cmd);
        'value' cmd;
        'string' cmd;
    'begin'
        bashTool := "executed"
    'end';
    
    'comment' Main program;
    msgCount := 0;
    running := 'true';
    
    outstring(1, "NANOCODE | ALGOL 60 - The Ancestor (1960)");
    outline(1);
    outline(1);
    
    'while' running 'do'
    'begin'
        outstring(1, "> ");
        instring(1, input);
        
        'if' input = "" 'then'
            'comment' continue
        'else' 'if' input = "/q" 'then'
            running := 'false'
        'else' 'if' input = "/c" 'then'
        'begin'
            msgCount := 0;
            outstring(1, "* Cleared");
            outline(1)
        'end'
        'else'
        'begin'
            msgCount := msgCount + 1;
            messages[msgCount] := input;
            
            'comment' Response;
            outline(1);
            outstring(1, "* ALGOL: The common ancestor!");
            outline(1);
            outstring(1, "  Block structure, lexical scoping, BNF notation");
            outline(1);
            outline(1)
        'end'
    'end';
    
    outstring(1, "Goodbye!");
    outline(1)
'end'

'comment'
Why ALGOL for AI agents?

1. HISTORICAL FOUNDATION
   - First language with block structure
   - Introduced lexical scoping
   - BNF notation for syntax

2. INFLUENCED EVERYTHING
   - C, Pascal, Java, Python all descended from ALGOL
   - Understanding ALGOL = understanding language design
   - AI code generation needs language theory

3. STRUCTURED PROGRAMMING
   - No GOTO (well, minimal)
   - Procedures with parameters
   - Nested blocks

4. FORMAL METHODS
   - ALGOL was mathematically specified
   - Proof-oriented from the start
   - Verification concepts from 1960

In the AI era, ALGOL reminds us that good language
design principles are timeless. The ideas that made
ALGOL influential in 1960 still guide us today.
;
