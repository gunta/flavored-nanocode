#!/usr/bin/env nu
# nanocode - minimal claude code alternative (Nushell)
let KEY = $env.ANTHROPIC_API_KEY
let MODEL = ($env.MODEL? | default "claude-sonnet-4-20250514")

def run_tool [name: string, input: record] {
    match $name {
        "read" => { open $input.path | lines | enumerate | each { |r| $"($r.index + 1)| ($r.item)" } | str join "\n" }
        "write" => { $input.content | save -f $input.path; "ok" }
        "bash" => { ^sh -c $input.cmd | complete | get stdout }
        "glob" => { glob $"**/*($input.pat)*" | first 50 | str join "\n" }
        "grep" => { ^grep -rn $input.pat . | lines | first 50 | str join "\n" }
        _ => "unknown"
    }
}

let schema = '[{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},
{"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},
{"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}},
{"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
{"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}}]'

def ask [messages: list] {
    let body = {model: $MODEL, max_tokens: 4096, system: "Concise assistant", messages: $messages, tools: ($schema | from json)}
    http post -H [Content-Type application/json, anthropic-version 2023-06-01, x-api-key $KEY] https://api.anthropic.com/v1/messages ($body | to json)
}

print $"(ansi green_bold)nanocode(ansi reset) | (ansi white_dimmed)($MODEL)(ansi reset)\n"
mut messages = []

loop {
    let input = (input $"(ansi blue_bold)❯(ansi reset) " | str trim)
    if $input == "" { continue }
    if $input == "/q" { break }
    if $input == "/c" { $messages = []; print $"(ansi green)⏺ Cleared(ansi reset)"; continue }
    
    $messages = ($messages | append {role: user, content: $input})
    
    loop {
        let resp = (ask $messages)
        let content = $resp.content
        mut results = []
        
        for block in $content {
            if $block.type == "text" { print $"\n(ansi cyan)⏺(ansi reset) ($block.text)" }
            if $block.type == "tool_use" {
                print $"\n(ansi green)⏺ ($block.name)(ansi reset)"
                let result = (run_tool $block.name $block.input)
                print $"  (ansi white_dimmed)⎿ ($result | lines | first)(ansi reset)"
                $results = ($results | append {type: tool_result, tool_use_id: $block.id, content: $result})
            }
        }
        
        $messages = ($messages | append {role: assistant, content: $content})
        if ($results | is-empty) { break }
        $messages = ($messages | append {role: user, content: $results})
    }
    print ""
}
