#!/usr/bin/env lua
-- nanocode - minimal claude code alternative (Lua)
local json = require("cjson")
local http = require("socket.http")
local ltn12 = require("ltn12")

local KEY = os.getenv("ANTHROPIC_API_KEY")
local MODEL = os.getenv("MODEL") or "claude-sonnet-4-20250514"
local R, B, D, C, G, BL = "\27[0m", "\27[1m", "\27[2m", "\27[36m", "\27[32m", "\27[34m"

local function read_file(path)
    local f = io.open(path, "r")
    if not f then return "error: file not found" end
    local lines = {}
    local i = 1
    for line in f:lines() do lines[#lines+1] = i .. "| " .. line; i = i + 1 end
    f:close()
    return table.concat(lines, "\n")
end

local tools = {
    read = function(a) return read_file(a.path) end,
    write = function(a) local f = io.open(a.path, "w"); if f then f:write(a.content); f:close(); return "ok" end; return "error" end,
    edit = function(a) local f = io.open(a.path, "r"); if not f then return "error" end; local t = f:read("*a"); f:close()
        if not t:find(a.old, 1, true) then return "error: not found" end
        f = io.open(a.path, "w"); f:write((t:gsub(a.old, a.new, 1))); f:close(); return "ok" end,
    glob = function(a) local h = io.popen("find . -name '" .. a.pat .. "' 2>/dev/null | head -50"); local r = h:read("*a"); h:close(); return r ~= "" and r or "none" end,
    grep = function(a) local h = io.popen("grep -rn '" .. a.pat .. "' . 2>/dev/null | head -50"); local r = h:read("*a"); h:close(); return r ~= "" and r or "none" end,
    bash = function(a) local h = io.popen(a.cmd .. " 2>&1"); local r = h:read("*a"); h:close(); return r end,
}

local schema = {
    {name="read", description="Read file", input_schema={type="object", properties={path={type="string"}}, required={"path"}}},
    {name="write", description="Write file", input_schema={type="object", properties={path={type="string"}, content={type="string"}}, required={"path", "content"}}},
    {name="edit", description="Edit file", input_schema={type="object", properties={path={type="string"}, old={type="string"}, new={type="string"}}, required={"path", "old", "new"}}},
    {name="glob", description="Find files", input_schema={type="object", properties={pat={type="string"}}, required={"pat"}}},
    {name="grep", description="Search", input_schema={type="object", properties={pat={type="string"}}, required={"pat"}}},
    {name="bash", description="Run command", input_schema={type="object", properties={cmd={type="string"}}, required={"cmd"}}},
}

local function ask(messages)
    local body = json.encode({model=MODEL, max_tokens=4096, system="Concise coding assistant", messages=messages, tools=schema})
    local resp = {}
    http.request{url="https://api.anthropic.com/v1/messages", method="POST", headers={["Content-Type"]="application/json", ["anthropic-version"]="2023-06-01", ["x-api-key"]=KEY, ["Content-Length"]=#body}, source=ltn12.source.string(body), sink=ltn12.sink.table(resp)}
    return json.decode(table.concat(resp))
end

print(B .. "nanocode" .. R .. " | " .. D .. MODEL .. R .. "\n")
local messages = {}

while true do
    io.write(B .. BL .. "❯" .. R .. " "); io.flush()
    local input = io.read()
    if not input or input == "/q" then break end
    if input == "" then goto continue end
    if input == "/c" then messages = {}; print(G .. "⏺ Cleared" .. R); goto continue end

    messages[#messages+1] = {role="user", content=input}

    while true do
        local resp = ask(messages)
        local content = resp.content or {}
        local results = {}

        for _, block in ipairs(content) do
            if block.type == "text" then print("\n" .. C .. "⏺" .. R .. " " .. block.text) end
            if block.type == "tool_use" then
                print("\n" .. G .. "⏺ " .. block.name .. R)
                local result = tools[block.name](block.input)
                print("  " .. D .. "⎿ " .. (result:match("[^\n]+") or "") .. R)
                results[#results+1] = {type="tool_result", tool_use_id=block.id, content=result}
            end
        end

        messages[#messages+1] = {role="assistant", content=content}
        if #results == 0 then break end
        messages[#messages+1] = {role="user", content=results}
    end
    print()
    ::continue::
end
