--!strict
-- nanocode - minimal claude code alternative (Luau - Roblox's typed Lua)
-- Note: This is designed for Roblox/Lune runtime
-- lune run nanocode.luau

local HttpService = game:GetService("HttpService") -- or use Lune's net library
local KEY = os.getenv("ANTHROPIC_API_KEY") or ""
local MODEL = os.getenv("MODEL") or "claude-sonnet-4-20250514"

local R, B, D, C, G, BL = "\27[0m", "\27[1m", "\27[2m", "\27[36m", "\27[32m", "\27[34m"

type ToolInput = {path: string?, content: string?, cmd: string?, pat: string?}
type Block = {type: string, text: string?, name: string?, id: string?, input: ToolInput?}

local function tool(name: string, input: ToolInput): string
    if name == "read" then
        local file = io.open(input.path or "", "r")
        if not file then return "error: file not found" end
        local lines = {}
        local i = 1
        for line in file:lines() do
            table.insert(lines, i .. "| " .. line)
            i += 1
        end
        file:close()
        return table.concat(lines, "\n")
    elseif name == "write" then
        local file = io.open(input.path or "", "w")
        if not file then return "error: cannot write" end
        file:write(input.content or "")
        file:close()
        return "ok"
    elseif name == "bash" then
        local handle = io.popen(input.cmd or "")
        if not handle then return "error" end
        local result = handle:read("*a")
        handle:close()
        return result
    elseif name == "glob" then
        local handle = io.popen("find . -name '" .. (input.pat or "*") .. "' | head -50")
        if not handle then return "none" end
        local result = handle:read("*a")
        handle:close()
        return result ~= "" and result or "none"
    end
    return "unknown"
end

local schema = {
    {name = "read", description = "Read file", input_schema = {type = "object", properties = {path = {type = "string"}}, required = {"path"}}},
    {name = "write", description = "Write file", input_schema = {type = "object", properties = {path = {type = "string"}, content = {type = "string"}}, required = {"path", "content"}}},
    {name = "bash", description = "Run command", input_schema = {type = "object", properties = {cmd = {type = "string"}}, required = {"cmd"}}},
    {name = "glob", description = "Find files", input_schema = {type = "object", properties = {pat = {type = "string"}}, required = {"pat"}}},
}

local function ask(messages: {{role: string, content: any}}): {content: {Block}}
    local body = HttpService:JSONEncode({
        model = MODEL,
        max_tokens = 4096,
        system = "Concise coding assistant",
        messages = messages,
        tools = schema
    })
    
    local response = HttpService:RequestAsync({
        Url = "https://api.anthropic.com/v1/messages",
        Method = "POST",
        Headers = {
            ["Content-Type"] = "application/json",
            ["anthropic-version"] = "2023-06-01",
            ["x-api-key"] = KEY
        },
        Body = body
    })
    
    return HttpService:JSONDecode(response.Body)
end

print(B .. "nanocode" .. R .. " | " .. D .. "Luau + " .. MODEL .. R .. "\n")

local messages: {{role: string, content: any}} = {}

while true do
    io.write(B .. BL .. "❯" .. R .. " ")
    local input = io.read()
    if not input then break end
    input = input:gsub("^%s*(.-)%s*$", "%1") -- trim
    
    if input == "" then continue end
    if input == "/q" then break end
    if input == "/c" then
        messages = {}
        print(G .. "⏺ Cleared" .. R)
        continue
    end
    
    table.insert(messages, {role = "user", content = input})
    
    while true do
        local resp = ask(messages)
        local content = resp.content or {}
        local results: {{type: string, tool_use_id: string, content: string}} = {}
        
        for _, block in ipairs(content) do
            if block.type == "text" then
                print("\n" .. C .. "⏺" .. R .. " " .. (block.text or ""))
            elseif block.type == "tool_use" then
                local name = block.name or ""
                print("\n" .. G .. "⏺ " .. name .. R)
                local result = tool(name, block.input or {})
                print("  " .. D .. "⎿ " .. result:split("\n")[1]:sub(1, 60) .. R)
                table.insert(results, {type = "tool_result", tool_use_id = block.id or "", content = result})
            end
        end
        
        table.insert(messages, {role = "assistant", content = content})
        if #results == 0 then break end
        table.insert(messages, {role = "user", content = results})
    end
    print()
end
