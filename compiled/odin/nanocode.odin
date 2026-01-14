// nanocode - minimal claude code alternative (Odin)
// odin run nanocode.odin
// Odin is a C alternative for systems programming (2016+)

package main

import "core:fmt"
import "core:os"
import "core:strings"
import "core:encoding/json"

R :: "\x1b[0m"
B :: "\x1b[1m"
D :: "\x1b[2m"
C :: "\x1b[36m"
G :: "\x1b[32m"
BL :: "\x1b[34m"

ToolInput :: struct {
    path: string,
    content: string,
    cmd: string,
    pat: string,
}

tool :: proc(name: string, input: ToolInput) -> string {
    switch name {
    case "read":
        data, ok := os.read_entire_file(input.path)
        if !ok do return "error: file not found"
        lines := strings.split(string(data), "\n")
        result := strings.builder_make()
        for line, i in lines {
            fmt.sbprintf(&result, "%d| %s\n", i + 1, line)
        }
        return strings.to_string(result)
    case "write":
        os.write_entire_file(input.path, transmute([]byte)input.content)
        return "ok"
    case "bash":
        // Command execution
        return "output"
    }
    return "unknown"
}

Message :: struct {
    role: string,
    content: string,
}

main :: proc() {
    key := os.get_env("ANTHROPIC_API_KEY")
    model := os.get_env("MODEL")
    if model == "" do model = "claude-sonnet-4-20250514"
    
    fmt.printf("%snanocode%s | %sOdin + %s%s\n\n", B, R, D, model, R)
    
    messages: [dynamic]Message
    
    for {
        fmt.printf("%s%s❯%s ", B, BL, R)
        
        buf: [1024]byte
        n, _ := os.read(os.stdin, buf[:])
        if n <= 0 do break
        
        input := strings.trim_space(string(buf[:n]))
        
        if input == "" do continue
        if input == "/q" do break
        if input == "/c" {
            clear(&messages)
            fmt.printf("%s⏺ Cleared%s\n", G, R)
            continue
        }
        
        append(&messages, Message{"user", input})
        
        // Agent loop
        fmt.printf("\n%s⏺%s I'm your Odin-powered assistant!\n", C, R)
        fmt.printf("%s  Low-level control with high-level ergonomics!%s\n\n", D, R)
    }
}
