#!/usr/bin/env -S awk -f
# nanocode - minimal claude code alternative (AWK)
# Requires: curl, jq
BEGIN {
    KEY = ENVIRON["ANTHROPIC_API_KEY"]
    MODEL = ENVIRON["MODEL"] ? ENVIRON["MODEL"] : "claude-sonnet-4-20250514"
    printf "\033[1mnanocode\033[0m | \033[2m%s\033[0m\n\n", MODEL
    MSGS = "[]"
    while (1) {
        printf "\033[1m\033[34m❯\033[0m "
        if ((getline input < "/dev/stdin") <= 0) break
        gsub(/^[ \t]+|[ \t]+$/, "", input)
        if (input == "") continue
        if (input == "/q") exit
        if (input == "/c") { MSGS = "[]"; print "\033[32m⏺ Cleared\033[0m"; continue }
        
        cmd = "jq -nc --arg m \"" input "\" --argjson msgs '" MSGS "' '$msgs + [{role:\"user\",content:$m}]'"
        cmd | getline MSGS; close(cmd)
        
        while (1) {
            body_cmd = "jq -nc --arg model \"" MODEL "\" --argjson msgs '" MSGS "' " \
                "'{model:$model,max_tokens:4096,system:\"Concise assistant\",messages:$msgs," \
                "tools:[{name:\"read\",description:\"Read\",input_schema:{type:\"object\",properties:{path:{type:\"string\"}},required:[\"path\"]}}," \
                "{name:\"bash\",description:\"Run\",input_schema:{type:\"object\",properties:{cmd:{type:\"string\"}},required:[\"cmd\"]}}]}'"
            body_cmd | getline body; close(body_cmd)
            
            curl = "curl -s https://api.anthropic.com/v1/messages " \
                "-H 'Content-Type: application/json' " \
                "-H 'anthropic-version: 2023-06-01' " \
                "-H 'x-api-key: " KEY "' " \
                "-d '" body "'"
            
            resp = ""; while ((curl | getline line) > 0) resp = resp line; close(curl)
            
            text_cmd = "echo '" resp "' | jq -r '.content[]? | select(.type==\"text\") | .text'"
            while ((text_cmd | getline txt) > 0) printf "\n\033[36m⏺\033[0m %s", txt; close(text_cmd)
            
            tool_cmd = "echo '" resp "' | jq -r '.content[]? | select(.type==\"tool_use\") | \"\\(.name)|\\(.id)|\\(.input.path // .input.cmd)\"'"
            has_tools = 0
            while ((tool_cmd | getline toolinfo) > 0) {
                has_tools = 1
                split(toolinfo, parts, "|")
                printf "\n\033[32m⏺ %s\033[0m\n", parts[1]
                if (parts[1] == "read") { result_cmd = "nl -ba \"" parts[3] "\" | head -50" }
                else { result_cmd = parts[3] " 2>&1 | head -50" }
                result_cmd | getline result; close(result_cmd)
                printf "  \033[2m⎿ %s\033[0m\n", result
            }
            close(tool_cmd)
            
            content_cmd = "echo '" resp "' | jq -c .content"
            content_cmd | getline content; close(content_cmd)
            MSGS = MSGS; sub(/]$/, ",{\"role\":\"assistant\",\"content\":" content "}]", MSGS)
            
            if (!has_tools) break
        }
        print ""
    }
}
