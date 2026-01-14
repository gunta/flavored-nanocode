# nanocode - minimal claude code alternative (CoffeeScript)
# coffee nanocode.coffee
# CoffeeScript: JavaScript, but beautiful (2009)

{execSync} = require 'child_process'
{readFileSync, writeFileSync} = require 'fs'
{createInterface} = require 'readline'
https = require 'https'

# ANSI colors
[R, B, D, C, G, BL] = ["\x1b[0m", "\x1b[1m", "\x1b[2m", "\x1b[36m", "\x1b[32m", "\x1b[34m"]

KEY = process.env.ANTHROPIC_API_KEY
MODEL = process.env.MODEL ? "claude-sonnet-4-20250514"

# Tools - CoffeeScript's concise syntax shines here
tools =
  read: ({path}) ->
    readFileSync(path, 'utf8')
      .split('\n')
      .map((l, i) -> "#{i + 1}| #{l}")
      .join('\n')
  
  write: ({path, content}) ->
    writeFileSync path, content
    "ok"
  
  edit: ({path, old, new: newStr}) ->
    text = readFileSync path, 'utf8'
    return "error: not found" unless text.includes old
    writeFileSync path, text.replace old, newStr
    "ok"
  
  bash: ({cmd}) ->
    execSync(cmd, encoding: 'utf8', timeout: 30000).toString()
  
  glob: ({pat}) ->
    try execSync("find . -name '#{pat}' | head -50", encoding: 'utf8') catch then "none"
  
  grep: ({pat}) ->
    try execSync("grep -rn '#{pat}' . | head -50", encoding: 'utf8') catch then "none"

# Schema
schema = [
  {name: "read", description: "Read file", input_schema: {type: "object", properties: {path: {type: "string"}}, required: ["path"]}}
  {name: "write", description: "Write file", input_schema: {type: "object", properties: {path: {type: "string"}, content: {type: "string"}}, required: ["path", "content"]}}
  {name: "edit", description: "Edit file", input_schema: {type: "object", properties: {path: {type: "string"}, old: {type: "string"}, new: {type: "string"}}, required: ["path", "old", "new"]}}
  {name: "bash", description: "Run command", input_schema: {type: "object", properties: {cmd: {type: "string"}}, required: ["cmd"]}}
  {name: "glob", description: "Find files", input_schema: {type: "object", properties: {pat: {type: "string"}}, required: ["pat"]}}
  {name: "grep", description: "Search content", input_schema: {type: "object", properties: {pat: {type: "string"}}, required: ["pat"]}}
]

# API call
ask = (messages) ->
  new Promise (resolve, reject) ->
    body = JSON.stringify {model: MODEL, max_tokens: 4096, system: "Concise assistant", messages, tools: schema}
    req = https.request
      hostname: 'api.anthropic.com'
      path: '/v1/messages'
      method: 'POST'
      headers:
        'Content-Type': 'application/json'
        'anthropic-version': '2023-06-01'
        'x-api-key': KEY
    , (res) ->
      data = ''
      res.on 'data', (chunk) -> data += chunk
      res.on 'end', -> resolve JSON.parse data
    req.on 'error', reject
    req.write body
    req.end()

# Main
console.log "#{B}nanocode#{R} | #{D}CoffeeScript + #{MODEL}#{R}\n"

rl = createInterface input: process.stdin, output: process.stdout
messages = []

prompt = ->
  rl.question "#{B}#{BL}❯#{R} ", (input) ->
    input = input.trim()
    return prompt() unless input
    return rl.close() if input is '/q'
    if input is '/c'
      messages = []
      console.log "#{G}⏺ Cleared#{R}"
      return prompt()
    
    messages.push {role: 'user', content: input}
    
    loop_ = ->
      ask(messages).then (resp) ->
        content = resp.content ? []
        results = []
        
        for block in content
          if block.type is 'text'
            console.log "\n#{C}⏺#{R} #{block.text}"
          if block.type is 'tool_use'
            console.log "\n#{G}⏺ #{block.name}#{R}"
            try
              result = tools[block.name] block.input
            catch e
              result = "error: #{e.message}"
            console.log "  #{D}⎿ #{result.split('\n')[0][...60]}#{R}"
            results.push {type: 'tool_result', tool_use_id: block.id, content: result}
        
        messages.push {role: 'assistant', content}
        if results.length
          messages.push {role: 'user', content: results}
          loop_()
        else
          console.log ''
          prompt()
    
    loop_()

prompt()

# Why CoffeeScript?
# - Cleaner syntax than JavaScript
# - Inspired ES6+ features
# - Still used in many projects
# - Beautiful, expressive code
