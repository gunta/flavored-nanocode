#!/bin/sh
# nanocode - minimal claude code (POSIX sh + curl + jq)
# Tools: read, write, edit, glob, grep, bash; agentic loop matching python baseline

set -e

K="${OPENROUTER_API_KEY:-$ANTHROPIC_API_KEY}"
API_DEFAULT="https://api.anthropic.com/v1/messages"
[ -n "$OPENROUTER_API_KEY" ] && API_DEFAULT="https://openrouter.ai/api/v1/messages"
A="${API_URL:-$API_DEFAULT}"
M="${MODEL:-${OPENROUTER_API_KEY:+anthropic/claude-opus-4-5}}"
M="${M:-claude-opus-4-5}"

TOOLS='[
  {"name":"read","description":"Read file","input_schema":{"type":"object","properties":{"path":{"type":"string"},"offset":{"type":"integer"},"limit":{"type":"integer"}},"required":["path"]}},
  {"name":"write","description":"Write file","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},
  {"name":"edit","description":"Replace text","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"},"all":{"type":"boolean"}},"required":["path","old","new"]}},
  {"name":"glob","description":"Find files","input_schema":{"type":"object","properties":{"pat":{"type":"string"},"path":{"type":"string"}}, "required":["pat"]}},
  {"name":"grep","description":"Search files","input_schema":{"type":"object","properties":{"pat":{"type":"string"},"path":{"type":"string"}}, "required":["pat"]}},
  {"name":"bash","description":"Run command","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}}, "required":["cmd"]}}
]'

echo "nanocode | $M | $A"
H='[]'

while printf "❯ " && IFS= read -r Q; do
  [ "$Q" = "/q" ] && break
  if [ "$Q" = "/c" ]; then H='[]'; echo "Cleared"; continue; fi
  H=$(echo "$H" | jq --arg q "$Q" '.+[{"role":"user","content":$q}]')
  while :; do
    AUTH_HEADER=$([ -n "$OPENROUTER_API_KEY" ] && printf 'Authorization: Bearer %s' "$K" || printf 'x-api-key: %s' "$K")
    R=$(curl -s "$A" \
      -H "Content-Type: application/json" \
      -H "anthropic-version: 2023-06-01" \
      -H "$AUTH_HEADER" \
      -d "$(jq -n --arg m "$M" --argjson h "$H" --argjson t "$TOOLS" '{model:$m,max_tokens:8192,system:"Concise coding assistant",messages:$h,tools:$t}')")

    C=$(echo "$R" | jq -c .content)
    RS='[]'

    for i in $(seq 0 $(($(echo "$C" | jq length)-1))); do
      B=$(echo "$C" | jq -c ".[$i]")
      TYPE=$(echo "$B" | jq -r .type)
      [ "$TYPE" = "text" ] && echo "⏺ $(echo "$B" | jq -r .text)"

      if [ "$TYPE" = "tool_use" ]; then
        N=$(echo "$B" | jq -r .name)
        I=$(echo "$B" | jq -c .input)
        ID=$(echo "$B" | jq -r .id)
        echo "⏺ $N"
        case "$N" in
          read)
            PATH_ARG=$(echo "$I" | jq -r .path)
            OFF=$(echo "$I" | jq -r '.offset // 0')
            LIM=$(echo "$I" | jq -r '.limit // 0')
            O=$(awk -v off="$OFF" -v lim="$LIM" 'NR>off {if (lim==0 || NR<=off+lim) printf "%d| %s\n", NR, $0}' "$PATH_ARG" 2>/dev/null || true)
            ;;
          write)
            P=$(echo "$I" | jq -r .path)
            echo -n "$(echo "$I" | jq -r .content)" > "$P"
            O="ok"
            ;;
          edit)
            P=$(echo "$I" | jq -r .path)
            OLD=$(echo "$I" | jq -r .old)
            NEW=$(echo "$I" | jq -r .new)
            ALL=$(echo "$I" | jq -r '.all // false')
            TXT=$(cat "$P" 2>/dev/null || true)
            COUNT=$(printf "%s" "$TXT" | grep -F -o "$OLD" | wc -l | tr -d ' ')
            if [ "$COUNT" -eq 0 ]; then O="error: old_string not found";
            elif [ "$COUNT" -gt 1 ] && [ "$ALL" != "true" ]; then O="error: old_string appears $COUNT times, use all=true";
            else
              if [ "$ALL" = "true" ]; then O=$(printf "%s" "$TXT" | sed "s|$OLD|$NEW|g"); else O=$(printf "%s" "$TXT" | sed "0,/$OLD/{s//$NEW/}"); fi
              printf "%s" "$O" > "$P"
              O="ok"
            fi
            ;;
          glob)
            BASE=$(echo "$I" | jq -r '.path // "."')
            PAT=$(echo "$I" | jq -r .pat)
            O=$(find "$BASE" -name "$PAT" -maxdepth 10 2>/dev/null | sort | head -50)
            ;;
          grep)
            BASE=$(echo "$I" | jq -r '.path // "."')
            PAT=$(echo "$I" | jq -r .pat)
            O=$(find "$BASE" -type f 2>/dev/null | xargs -I{} sh -c "grep -n \"${PAT}\" \"{}\" 2>/dev/null" | head -50)
            [ -z "$O" ] && O="none"
            ;;
          bash)
            O=$(eval "$(echo "$I" | jq -r .cmd)" 2>&1)
            ;;
        esac
        PREVIEW=$(printf "%s" "$O" | head -n1 | cut -c1-80)
        echo "  ⎿ $PREVIEW"
        RS=$(echo "$RS" | jq --arg id "$ID" --arg o "$O" '.+[{"type":"tool_result","tool_use_id":$id,"content":$o}]')
      fi
    done

    H=$(echo "$H" | jq --argjson c "$C" '.+[{"role":"assistant","content":$c}]')
    [ "$RS" = "[]" ] && break
    H=$(echo "$H" | jq --argjson r "$RS" '.+[{"role":"user","content":$r}]')
  done
done
