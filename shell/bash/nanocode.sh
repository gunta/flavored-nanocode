#!/usr/bin/env bash
# nanocode - minimal claude code alternative (Bash)
set -e
API="${OPENROUTER_API_KEY:+https://openrouter.ai/api/v1/messages}"
API="${API:-https://api.anthropic.com/v1/messages}"
KEY="${OPENROUTER_API_KEY:-$ANTHROPIC_API_KEY}"
MODEL="${MODEL:-${OPENROUTER_API_KEY:+anthropic/claude-opus-4}}"
MODEL="${MODEL:-claude-sonnet-4-20250514}"
AUTH="${OPENROUTER_API_KEY:+Authorization: Bearer $KEY}"
AUTH="${AUTH:-x-api-key: $KEY}"

R='\033[0m' B='\033[1m' D='\033[2m' C='\033[36m' G='\033[32m'
echo -e "${B}nanocode${R} | ${D}$MODEL | $PWD${R}\n"

SCHEMA='[{"name":"read","description":"Read file","input_schema":{"type":"object","properties":{"path":{"type":"string"},"offset":{"type":"integer"},"limit":{"type":"integer"}},"required":["path"]}},{"name":"write","description":"Write file","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},{"name":"edit","description":"Edit file","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"}},"required":["path","old","new"]}},{"name":"glob","description":"Find files","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},{"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},{"name":"bash","description":"Run command","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}]'

run_tool() {
  local name="$1" input="$2"
  case "$name" in
    read) local p=$(echo "$input"|jq -r .path); nl -ba "$p" 2>/dev/null | head -50 ;;
    write) local p=$(echo "$input"|jq -r .path) c=$(echo "$input"|jq -r .content); echo "$c" > "$p" && echo "ok" ;;
    edit) local p=$(echo "$input"|jq -r .path) o=$(echo "$input"|jq -r .old) n=$(echo "$input"|jq -r .new); sed -i '' "s|$o|$n|" "$p" 2>/dev/null && echo "ok" || echo "error" ;;
    glob) find . -name "$(echo "$input"|jq -r .pat)" 2>/dev/null | head -50 ;;
    grep) grep -rn "$(echo "$input"|jq -r .pat)" . 2>/dev/null | head -50 ;;
    bash) eval "$(echo "$input"|jq -r .cmd)" 2>&1 | head -100 ;;
  esac
}

MSGS='[]'
while true; do
  echo -en "${B}\033[34m❯${R} "; read -r input || break
  [[ -z "$input" ]] && continue
  [[ "$input" == "/q" ]] && break
  [[ "$input" == "/c" ]] && MSGS='[]' && echo -e "${G}⏺ Cleared${R}" && continue
  
  MSGS=$(echo "$MSGS" | jq --arg m "$input" '. + [{"role":"user","content":$m}]')
  
  while true; do
    BODY=$(jq -n --arg m "$MODEL" --argjson msgs "$MSGS" --argjson tools "$SCHEMA" \
      '{model:$m,max_tokens:4096,system:"Concise coding assistant. cwd: '"$PWD"'",messages:$msgs,tools:$tools}')
    
    RESP=$(curl -s "$API" -H "Content-Type: application/json" -H "anthropic-version: 2023-06-01" -H "$AUTH" -d "$BODY")
    CONTENT=$(echo "$RESP" | jq -c '.content // []')
    
    RESULTS='[]'
    echo "$CONTENT" | jq -c '.[]' | while read -r block; do
      TYPE=$(echo "$block" | jq -r .type)
      if [[ "$TYPE" == "text" ]]; then
        echo -e "\n${C}⏺${R} $(echo "$block" | jq -r .text)"
      elif [[ "$TYPE" == "tool_use" ]]; then
        NAME=$(echo "$block" | jq -r .name)
        INPUT=$(echo "$block" | jq -c .input)
        ID=$(echo "$block" | jq -r .id)
        echo -e "\n${G}⏺ $NAME${R}(${D}${INPUT:0:50}${R})"
        RESULT=$(run_tool "$NAME" "$INPUT")
        echo -e "  ${D}⎿ ${RESULT%%$'\n'*}${R}"
        echo "$ID|$RESULT" >> /tmp/nc_results
      fi
    done
    
    MSGS=$(echo "$MSGS" | jq --argjson c "$CONTENT" '. + [{"role":"assistant","content":$c}]')
    
    [[ ! -f /tmp/nc_results ]] && break
    RESULTS='[]'
    while IFS='|' read -r id result; do
      RESULTS=$(echo "$RESULTS" | jq --arg id "$id" --arg r "$result" '. + [{"type":"tool_result","tool_use_id":$id,"content":$r}]')
    done < /tmp/nc_results
    rm -f /tmp/nc_results
    [[ "$RESULTS" == "[]" ]] && break
    MSGS=$(echo "$MSGS" | jq --argjson r "$RESULTS" '. + [{"role":"user","content":$r}]')
  done
  echo
done
