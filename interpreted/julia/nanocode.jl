#!/usr/bin/env julia
# nanocode - minimal claude code alternative (Julia)
using HTTP, JSON3

KEY = ENV["ANTHROPIC_API_KEY"]
MODEL = get(ENV, "MODEL", "claude-sonnet-4-20250514")
R, B, D, C, G, BL = "\e[0m", "\e[1m", "\e[2m", "\e[36m", "\e[32m", "\e[34m"

tools = Dict(
    "read" => a -> join(["$i| $l" for (i,l) in enumerate(readlines(a["path"]))], "\n"),
    "write" => a -> (write(a["path"], a["content"]); "ok"),
    "edit" => a -> begin t = read(a["path"], String); occursin(a["old"], t) ? (write(a["path"], replace(t, a["old"] => a["new"], count=1)); "ok") : "error: not found" end,
    "glob" => a -> read(`find . -name $(a["pat"])`, String) |> s -> join(split(s, "\n")[1:min(50,end)], "\n"),
    "grep" => a -> try read(`grep -rn $(a["pat"]) .`, String) |> s -> join(split(s, "\n")[1:min(50,end)], "\n") catch; "none" end,
    "bash" => a -> read(`sh -c $(a["cmd"])`, String)
)

schema = JSON3.read("""[{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},
{"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},
{"name":"edit","description":"Edit","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"}},"required":["path","old","new"]}},
{"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
{"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
{"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}]""")

function ask(messages)
    resp = HTTP.post("https://api.anthropic.com/v1/messages",
        ["Content-Type" => "application/json", "anthropic-version" => "2023-06-01", "x-api-key" => KEY],
        JSON3.write(Dict("model" => MODEL, "max_tokens" => 4096, "system" => "Concise assistant", "messages" => messages, "tools" => schema)))
    JSON3.read(String(resp.body))
end

println("$(B)nanocode$(R) | $(D)$(MODEL)$(R)\n")
messages = []

while true
    print("$(B)$(BL)❯$(R) ")
    input = strip(readline())
    isempty(input) && continue
    input == "/q" && break
    input == "/c" && (empty!(messages); println("$(G)⏺ Cleared$(R)"); continue)
    
    push!(messages, Dict("role" => "user", "content" => input))
    
    while true
        resp = ask(messages)
        content = resp.content
        results = []
        
        for block in content
            block.type == "text" && println("\n$(C)⏺$(R) $(block.text)")
            if block.type == "tool_use"
                name = block.name
                println("\n$(G)⏺ $(name)$(R)")
                result = try tools[name](Dict(pairs(block.input))) catch e "error: $e" end
                println("  $(D)⎿ $(split(result, "\n")[1])$(R)")
                push!(results, Dict("type" => "tool_result", "tool_use_id" => block.id, "content" => result))
            end
        end
        
        push!(messages, Dict("role" => "assistant", "content" => content))
        isempty(results) && break
        push!(messages, Dict("role" => "user", "content" => results))
    end
    println()
end
