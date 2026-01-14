#!/usr/bin/env fish
# nanocode - minimal claude code alternative (Fish)
set -g KEY $ANTHROPIC_API_KEY
set -g MODEL (test -n "$MODEL"; and echo $MODEL; or echo "claude-sonnet-4-20250514")
set -g SCHEMA '[{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},{"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}},{"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}}]'

function run_tool
    switch $argv[1]
        case read
            set -l path (echo $argv[2] | jq -r .path)
            nl -ba $path 2>/dev/null | head -50
        case bash
            set -l cmd (echo $argv[2] | jq -r .cmd)
            eval $cmd 2>&1 | head -100
        case glob
            set -l pat (echo $argv[2] | jq -r .pat)
            find . -name "$pat" 2>/dev/null | head -50
    end
end

echo -e "\e[1mnanocode\e[0m | \e[2m$MODEL\e[0m\n"
set -g MSGS '[]'

while true
    echo -ne "\e[1m\e[34m❯\e[0m "
    read -l input; or break
    test -z "$input"; and continue
    test "$input" = "/q"; and break
    if test "$input" = "/c"
        set MSGS '[]'
        echo -e "\e[32m⏺ Cleared\e[0m"
        continue
    end
    
    set MSGS (echo $MSGS | jq --arg m "$input" '. + [{"role":"user","content":$m}]')
    
    while true
        set -l body (jq -n --arg m "$MODEL" --argjson msgs "$MSGS" --argjson tools "$SCHEMA" \
            '{model:$m,max_tokens:4096,system:"Concise assistant",messages:$msgs,tools:$tools}')
        
        set -l resp (curl -s "https://api.anthropic.com/v1/messages" \
            -H "Content-Type: application/json" \
            -H "anthropic-version: 2023-06-01" \
            -H "x-api-key: $KEY" \
            -d "$body")
        
        set -l content (echo $resp | jq -c '.content // []')
        set -l results '[]'
        
        for block in (echo $content | jq -c '.[]')
            set -l type (echo $block | jq -r .type)
            if test "$type" = "text"
                echo -e "\n\e[36m⏺\e[0m "(echo $block | jq -r .text)
            else if test "$type" = "tool_use"
                set -l name (echo $block | jq -r .name)
                set -l input_json (echo $block | jq -c .input)
                set -l id (echo $block | jq -r .id)
                echo -e "\n\e[32m⏺ $name\e[0m"
                set -l result (run_tool $name $input_json)
                echo -e "  \e[2m⎿ "(echo $result | head -1)"\e[0m"
                set results (echo $results | jq --arg id "$id" --arg r "$result" '. + [{"type":"tool_result","tool_use_id":$id,"content":$r}]')
            end
        end
        
        set MSGS (echo $MSGS | jq --argjson c "$content" '. + [{"role":"assistant","content":$c}]')
        test "$results" = "[]"; and break
        set MSGS (echo $MSGS | jq --argjson r "$results" '. + [{"role":"user","content":$r}]')
    end
    echo
end
