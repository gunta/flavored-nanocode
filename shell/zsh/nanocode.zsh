#!/usr/bin/env zsh
# nanocode - minimal claude code alternative (Zsh)
setopt no_nomatch
API="${OPENROUTER_API_KEY:+https://openrouter.ai/api/v1/messages}"
: ${API:=https://api.anthropic.com/v1/messages}
KEY="${OPENROUTER_API_KEY:-$ANTHROPIC_API_KEY}"
MODEL="${MODEL:-${OPENROUTER_API_KEY:+anthropic/claude-opus-4}}"
: ${MODEL:=claude-sonnet-4-20250514}
AUTH="${OPENROUTER_API_KEY:+Authorization: Bearer $KEY}"
: ${AUTH:=x-api-key: $KEY}

print -P "%B%F{white}nanocode%f%b | %F{8}$MODEL | $PWD%f\n"

typeset -A tools
tools=(
  read 'nl -ba "$path" 2>/dev/null | head -50'
  write 'print -r -- "$content" > "$path" && echo ok'
  edit 'sed -i "" "s|$old|$new|" "$path" && echo ok'
  glob 'print -l **/*$pat*(N) | head -50'
  grep 'grep -rn "$pat" . 2>/dev/null | head -50'
  bash 'eval "$cmd" 2>&1 | head -100'
)

SCHEMA='[{"name":"read","description":"Read file","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},{"name":"write","description":"Write file","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},{"name":"edit","description":"Edit file","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"}},"required":["path","old","new"]}},{"name":"glob","description":"Find files","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},{"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},{"name":"bash","description":"Run command","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}]'

MSGS='[]'
while vared -p $'%B%F{blue}❯%f%b ' -c input; do
  [[ -z $input ]] && continue
  [[ $input == /q ]] && break
  [[ $input == /c ]] && MSGS='[]' && print -P "%F{green}⏺ Cleared%f" && input='' && continue
  
  MSGS=$(print -r -- $MSGS | jq --arg m "$input" '. + [{"role":"user","content":$m}]')
  
  while true; do
    BODY=$(jq -n --arg m "$MODEL" --argjson msgs "$MSGS" --argjson tools "$SCHEMA" \
      '{model:$m,max_tokens:4096,system:"Concise coding assistant",messages:$msgs,tools:$tools}')
    
    RESP=$(curl -s "$API" -H "Content-Type: application/json" -H "anthropic-version: 2023-06-01" -H "$AUTH" -d "$BODY")
    CONTENT=$(print -r -- $RESP | jq -c '.content // []')
    
    local results=()
    print -r -- $CONTENT | jq -c '.[]' | while read -r block; do
      local type=$(print -r -- $block | jq -r .type)
      if [[ $type == text ]]; then
        print -P "\n%F{cyan}⏺%f $(print -r -- $block | jq -r .text)"
      elif [[ $type == tool_use ]]; then
        local name=$(print -r -- $block | jq -r .name)
        local -A input_vars
        eval "$(print -r -- $block | jq -r '.input | to_entries | .[] | "local \(.key)=\(.value|@sh)"')"
        local id=$(print -r -- $block | jq -r .id)
        print -P "\n%F{green}⏺ $name%f"
        local result=$(eval "$tools[$name]" 2>&1)
        print -P "  %F{8}⎿ ${result%%$'\n'*}%f"
        results+=("$id|$result")
      fi
    done
    
    MSGS=$(print -r -- $MSGS | jq --argjson c "$CONTENT" '. + [{"role":"assistant","content":$c}]')
    [[ ${#results} -eq 0 ]] && break
    
    local tool_results='[]'
    for r in $results; do
      local id=${r%%|*} content=${r#*|}
      tool_results=$(print -r -- $tool_results | jq --arg id "$id" --arg c "$content" '. + [{"type":"tool_result","tool_use_id":$id,"content":$c}]')
    done
    MSGS=$(print -r -- $MSGS | jq --argjson r "$tool_results" '. + [{"role":"user","content":$r}]')
  done
  print; input=''
done
