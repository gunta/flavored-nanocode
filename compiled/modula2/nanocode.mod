(* nanocode - minimal claude code alternative (Modula-2) *)
(* gm2 -o nanocode nanocode.mod && ./nanocode *)
(* Modula-2: Wirth's successor to Pascal (1978) *)

MODULE Nanocode;

FROM InOut IMPORT WriteString, WriteLn, ReadString, Write;
FROM Strings IMPORT Length, Compare;
FROM SYSTEM IMPORT ADR;

CONST
    MaxMessages = 100;
    MaxLength = 1024;

TYPE
    String = ARRAY [0..MaxLength-1] OF CHAR;
    Message = RECORD
        role: String;
        content: String;
    END;
    MessageArray = ARRAY [0..MaxMessages-1] OF Message;

VAR
    messages: MessageArray;
    msgCount: CARDINAL;
    input: String;
    running: BOOLEAN;
    (* ANSI colors *)
    R, B, D, C, G, BL: String;

(* Tool procedures *)
PROCEDURE ReadTool(path: ARRAY OF CHAR): String;
VAR
    result: String;
BEGIN
    (* File reading would go here *)
    result := "1| file content";
    RETURN result;
END ReadTool;

PROCEDURE WriteTool(path, content: ARRAY OF CHAR): String;
VAR
    result: String;
BEGIN
    (* File writing would go here *)
    result := "ok";
    RETURN result;
END WriteTool;

PROCEDURE BashTool(cmd: ARRAY OF CHAR): String;
VAR
    result: String;
BEGIN
    (* Command execution would go here *)
    result := "executed";
    RETURN result;
END BashTool;

(* Main procedure *)
PROCEDURE Main;
BEGIN
    (* Initialize ANSI codes *)
    R := 33C + "[0m";
    B := 33C + "[1m";
    D := 33C + "[2m";
    C := 33C + "[36m";
    G := 33C + "[32m";
    BL := 33C + "[34m";
    
    msgCount := 0;
    running := TRUE;
    
    WriteString(B); WriteString("nanocode"); WriteString(R);
    WriteString(" | "); WriteString(D);
    WriteString("Modula-2 - Modules Done Right (1978)");
    WriteString(R); WriteLn;
    WriteLn;
    
    WHILE running DO
        WriteString(B); WriteString(BL);
        Write(">"); WriteString(R); WriteString(" ");
        
        ReadString(input);
        
        IF Length(input) = 0 THEN
            (* Continue *)
        ELSIF Compare(input, "/q") = 0 THEN
            running := FALSE;
        ELSIF Compare(input, "/c") = 0 THEN
            msgCount := 0;
            WriteString(G); WriteString("* Cleared"); WriteString(R);
            WriteLn;
        ELSE
            (* Add message *)
            messages[msgCount].role := "user";
            messages[msgCount].content := input;
            INC(msgCount);
            
            (* Response *)
            WriteLn;
            WriteString(C); WriteString("*"); WriteString(R);
            WriteString(" Modula-2: Modules as first-class citizens!");
            WriteLn;
            WriteString(D);
            WriteString("  Separate compilation, information hiding, coroutines");
            WriteString(R);
            WriteLn;
            WriteLn;
        END;
    END;
    
    WriteString("Goodbye!");
    WriteLn;
END Main;

BEGIN
    Main;
END Nanocode.

(* Why Modula-2 for AI agents?

1. MODULE SYSTEM
   - First language with proper modules
   - Influenced Ada, Python, Rust modules
   - Clean separation of interface/implementation

2. WIRTH'S VISION
   - Simple, elegant, powerful
   - "Make it as simple as possible, but no simpler"
   - Philosophy AI agents should follow

3. COROUTINES
   - Built-in coroutine support
   - Process/procedure abstraction
   - Async patterns before async existed

4. SYSTEMS PROGRAMMING
   - Low-level access when needed
   - High-level abstractions otherwise
   - Perfect for system tools

Niklaus Wirth designed languages that were simple yet powerful.
In the AI era, his design principles remain relevant:
- Simplicity over complexity
- Clarity over cleverness
- Modules for organization
*)
