#!/usr/bin/env nu
# nanocode - minimal claude code alternative (Nushell)
let KEY = (if ($env.OPENROUTER_API_KEY?) != null { $env.OPENROUTER_API_KEY } else { $env.ANTHROPIC_API_KEY })
let MODEL = ($env.MODEL? | default (if ($env.OPENROUTER_API_KEY?) != null { "anthropic/claude-opus-4-5" } else { "claude-opus-4-5" }))
let API = (if ($env.OPENROUTER_API_KEY?) != null { "https://openrouter.ai/api/v1/messages" } else { "https://api.anthropic.com/v1/messages" })
let AUTH_HEADER = (if ($env.OPENROUTER_API_KEY?) != null { $"Authorization: Bearer ($KEY)" } else { $"x-api-key: ($KEY)" })

def run_tool [name: string, input: record] {
    match $name {
        "read" => { 
            let off = ($input.offset? | default 0)
            let lim = ($input.limit? | default 0)
            open $input.path 
            | lines 
            | drop $off 
            | (if $lim == 0 { each { |r i| $"($off + $i + 1)| ($r)" } } else { take $lim | enumerate | each { |r| $"($off + $r.index + 1)| ($r.item)" } })
            | str join "\n"
        }
        "write" => { $input.content | save -f $input.path; "ok" }
        "edit" => { 
            let txt = (open $input.path | to text)
            let count = ($txt | str-find-all -c $input.old | length)
            if $count == 0 { "error: old_string not found" }
            else if $count > 1 and ($input.all? | default false) == false { $"error: old_string appears ($count) times, use all=true" }
            else {
                let updated = (if ($input.all? | default false) { $txt | str replace -a $input.old $input.new } else { $txt | str replace $input.old $input.new })
                $updated | save -f $input.path
                "ok"
            }
        }
        "bash" => { ^sh -c $input.cmd | complete | get stdout }
        "glob" => { 
            let base = ($input.path? | default ".")
            glob $"($base)/**/($input.pat)" | first 50 | str join "\n" 
        }
        "grep" => { 
            let base = ($input.path? | default ".")
            ^sh -c $"grep -rn \"($input.pat)\" \"($base)\" 2>/dev/null" | lines | first 50 | str join "\n"
        }
        _ => "unknown"
    }
}

let schema = '[{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"},"offset":{"type":"integer"},"limit":{"type":"integer"}},"required":["path"]}},
{"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},
{"name":"edit","description":"Replace","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"},"all":{"type":"boolean"}},"required":["path","old","new"]}},
{"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}},
{"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"},"path":{"type":"string"}},"required":["pat"]}},
{"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}}]'

def ask [messages: list] {
    let body = {model: $MODEL, max_tokens: 4096, system: "Concise assistant", messages: $messages, tools: ($schema | from json)}
    http post -H [Content-Type application/json, anthropic-version 2023-06-01, $AUTH_HEADER] $API ($body | to json)
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
