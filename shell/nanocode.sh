#!/bin/sh
# nanocode - minimal claude code (just curl + jq)
K="${ANTHROPIC_API_KEY}" M="${MODEL:-claude-sonnet-4-20250514}"
A="https://api.anthropic.com/v1/messages"
T='[{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},{"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},{"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}]'
echo "nanocode | $M"
H='[]'
while printf "❯ " && read -r Q; do
  [ "$Q" = "/q" ] && break
  [ "$Q" = "/c" ] && H='[]' && echo "Cleared" && continue
  H=$(echo "$H"|jq --arg q "$Q" '.+[{"role":"user","content":$q}]')
  while :; do
    R=$(curl -s "$A" -H "Content-Type: application/json" -H "anthropic-version: 2023-06-01" -H "x-api-key: $K" \
      -d "$(jq -n --arg m "$M" --argjson h "$H" --argjson t "$T" '{model:$m,max_tokens:4096,system:"Concise assistant",messages:$h,tools:$t}')")
    C=$(echo "$R"|jq -c .content)
    RS='[]'
    for i in $(seq 0 $(($(echo "$C"|jq length)-1))); do
      B=$(echo "$C"|jq -c ".[$i]")
      [ "$(echo "$B"|jq -r .type)" = "text" ] && echo "⏺ $(echo "$B"|jq -r .text)"
      if [ "$(echo "$B"|jq -r .type)" = "tool_use" ]; then
        N=$(echo "$B"|jq -r .name) I=$(echo "$B"|jq -c .input) ID=$(echo "$B"|jq -r .id)
        echo "⏺ $N"
        case $N in
          read) O=$(cat "$(echo "$I"|jq -r .path)" 2>&1);;
          write) echo "$(echo "$I"|jq -r .content)">"$(echo "$I"|jq -r .path)" && O="ok";;
          bash) O=$(eval "$(echo "$I"|jq -r .cmd)" 2>&1);;
        esac
        echo "  ⎿ ${O%%
*}"
        RS=$(echo "$RS"|jq --arg id "$ID" --arg o "$O" '.+[{"type":"tool_result","tool_use_id":$id,"content":$o}]')
      fi
    done
    H=$(echo "$H"|jq --argjson c "$C" '.+[{"role":"assistant","content":$c}]')
    [ "$RS" = "[]" ] && break
    H=$(echo "$H"|jq --argjson r "$RS" '.+[{"role":"user","content":$r}]')
  done
done
