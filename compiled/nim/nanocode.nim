# nanocode - minimal claude code alternative (Nim)
# nim c -r nanocode.nim
import std/[httpclient, json, os, osproc, strutils, sequtils, terminal]

let KEY = getEnv("ANTHROPIC_API_KEY")
let MODEL = getEnv("MODEL", "claude-sonnet-4-20250514")
const R = "\e[0m"; B = "\e[1m"; D = "\e[2m"; C = "\e[36m"; G = "\e[32m"; BL = "\e[34m"

proc tool(name: string, input: JsonNode): string =
  case name
  of "read": 
    try: readFile(input["path"].getStr).splitLines.mapIt($input["path"].getStr.readFile.splitLines.find(it) & "| " & it).join("\n")
    except: "error: " & getCurrentExceptionMsg()
  of "write": writeFile(input["path"].getStr, input["content"].getStr); "ok"
  of "edit":
    var t = readFile(input["path"].getStr)
    if input["old"].getStr notin t: return "error: not found"
    writeFile(input["path"].getStr, t.replace(input["old"].getStr, input["new"].getStr)); "ok"
  of "glob": execProcess("find . -name '" & input["pat"].getStr & "' | head -50")
  of "grep": execProcess("grep -rn '" & input["pat"].getStr & "' . | head -50")
  of "bash": execProcess(input["cmd"].getStr)
  else: "unknown"

let schema = parseJson("""[{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},
{"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},
{"name":"edit","description":"Edit","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"}},"required":["path","old","new"]}},
{"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
{"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
{"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}]""")

proc ask(messages: JsonNode): JsonNode =
  let client = newHttpClient()
  client.headers = newHttpHeaders({"Content-Type": "application/json", "anthropic-version": "2023-06-01", "x-api-key": KEY})
  let body = %*{"model": MODEL, "max_tokens": 4096, "system": "Concise assistant", "messages": messages, "tools": schema}
  parseJson(client.postContent("https://api.anthropic.com/v1/messages", $body))

echo B, "nanocode", R, " | ", D, MODEL, R, "\n"
var messages = newJArray()

while true:
  stdout.write B, BL, "❯", R, " "; stdout.flushFile
  let input = stdin.readLine.strip
  if input == "": continue
  if input == "/q": break
  if input == "/c": messages = newJArray(); echo G, "⏺ Cleared", R; continue
  
  messages.add(%*{"role": "user", "content": input})
  
  while true:
    let resp = ask(messages)
    let content = resp["content"]
    var results = newJArray()
    
    for block in content:
      if block["type"].getStr == "text": echo "\n", C, "⏺", R, " ", block["text"].getStr
      if block["type"].getStr == "tool_use":
        let name = block["name"].getStr
        echo "\n", G, "⏺ ", name, R
        let result = tool(name, block["input"])
        echo "  ", D, "⎿ ", result.splitLines[0], R
        results.add(%*{"type": "tool_result", "tool_use_id": block["id"], "content": result})
    
    messages.add(%*{"role": "assistant", "content": content})
    if results.len == 0: break
    messages.add(%*{"role": "user", "content": results})
  echo ""
