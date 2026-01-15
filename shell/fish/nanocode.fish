#!/usr/bin/env fish
# nanocode - minimal claude code alternative (Fish)
set -g KEY (string collect $OPENROUTER_API_KEY$ANTHROPIC_API_KEY)
set -g MODEL (test -n "$MODEL"; and echo $MODEL; or echo "claude-opus-4-5")
set -g API (test -n "$OPENROUTER_API_KEY"; and echo "https://openrouter.ai/api/v1/messages"; or echo "https://api.anthropic.com/v1/messages")
set -g AUTH_HEADER (test -n "$OPENROUTER_API_KEY"; and echo "Authorization: Bearer $OPENROUTER_API_KEY"; or echo "x-api-key: $ANTHROPIC_API_KEY")
set -g SCHEMA '[{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"},"offset":{"type":"integer"},"limit":{"type":"integer"}},"required":["path"]}},{"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},{"name":"edit","description":"Replace text","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"},"all":{"type":"boolean"}},"required":["path","old","new"]}},{"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"},"path":{"type":"string"}},"required":["pat"]}},{"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"},"path":{"type":"string"}},"required":["pat"]}},{"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}]'

function run_tool
    switch $argv[1]
        case read
            set -l path (echo $argv[2] | jq -r .path)
            set -l off (echo $argv[2] | jq -r '.offset // 0')
            set -l lim (echo $argv[2] | jq -r '.limit // 0')
            awk -v off=$off -v lim=$lim 'NR>off && (lim==0 || NR<=off+lim){printf "%d| %s\n",NR,$0}' $path 2>/dev/null | head -200
        case write
            set -l path (echo $argv[2] | jq -r .path)
            set -l content (echo $argv[2] | jq -r .content)
            echo -n $content > $path; or true
            echo ok
        case edit
            set -l path (echo $argv[2] | jq -r .path)
            set -l old (echo $argv[2] | jq -r .old)
            set -l new (echo $argv[2] | jq -r .new)
            set -l all (echo $argv[2] | jq -r '.all // false')
            set -l txt (cat $path 2>/dev/null)
            set -l count (printf "%s" "$txt" | grep -F -o "$old" | wc -l | tr -d ' ')
            if test $count -eq 0
                echo "error: old_string not found"
            else if test $count -gt 1 -a "$all" != "true"
                echo "error: old_string appears $count times, use all=true"
            else
                if test "$all" = "true"
                    printf "%s" "$txt" | sed "s|$old|$new|g" > $path
                else
                    printf "%s" "$txt" | sed "0,/$old/{s//$new/}" > $path
                end
                echo ok
            end
        case bash
            set -l cmd (echo $argv[2] | jq -r .cmd)
            eval $cmd 2>&1 | head -100
        case glob
            set -l base (echo $argv[2] | jq -r '.path // "."')
            set -l pat (echo $argv[2] | jq -r .pat)
            find $base -name "$pat" 2>/dev/null | head -50
        case grep
            set -l base (echo $argv[2] | jq -r '.path // "."')
            set -l pat (echo $argv[2] | jq -r .pat)
            find $base -type f 2>/dev/null | xargs -I{} sh -c "grep -n \"$pat\" \"{}\" 2>/dev/null" | head -50
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
        
        set -l resp (curl -s "$API" \
            -H "Content-Type: application/json" \
            -H "anthropic-version: 2023-06-01" \
            -H "$AUTH_HEADER" \
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
