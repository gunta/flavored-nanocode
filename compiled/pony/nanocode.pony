// nanocode - minimal claude code alternative (Pony)
// ponyc && ./nanocode
// Pony: Actor-based language with capabilities (2015+)

use "net"
use "files"
use "collections"

// ANSI colors
primitive Colors
  fun r(): String => "\x1b[0m"
  fun b(): String => "\x1b[1m"
  fun d(): String => "\x1b[2m"
  fun c(): String => "\x1b[36m"
  fun g(): String => "\x1b[32m"
  fun bl(): String => "\x1b[34m"

// Tool actor - isolated capability!
actor Tool
  let _env: Env
  
  new create(env: Env) =>
    _env = env
  
  be read_file(path: String, callback: {(String)} val) =>
    try
      let file = File.open(FilePath(_env.root as AmbientAuth, path)?)
      let content = file.read_string(file.size())
      let lines = content.split("\n")
      var result = ""
      var i: USize = 1
      for line in lines.values() do
        result = result + i.string() + "| " + line + "\n"
        i = i + 1
      end
      callback(result)
    else
      callback("error: file not found")
    end
  
  be write_file(path: String, content: String, callback: {(String)} val) =>
    try
      let file = File.create(FilePath(_env.root as AmbientAuth, path)?)
      file.write(content)
      callback("ok")
    else
      callback("error: cannot write")
    end
  
  be bash(cmd: String, callback: {(String)} val) =>
    // Command execution would go here
    callback("output")

// Message class
class Message
  let role: String
  let content: String
  
  new create(role': String, content': String) =>
    role = role'
    content = content'

// Main actor
actor Main
  let _env: Env
  let _tool: Tool
  var _messages: Array[Message] = Array[Message]
  
  new create(env: Env) =>
    _env = env
    _tool = Tool(env)
    
    // Print header
    env.out.print(Colors.b() + "nanocode" + Colors.r() + " | " + Colors.d() + "Pony - Actor Safety" + Colors.r())
    env.out.print("")
    
    prompt()
  
  be prompt() =>
    _env.out.write(Colors.b() + Colors.bl() + "❯" + Colors.r() + " ")
    _env.input(recover Notify(this) end)
  
  be handle_input(input: String) =>
    let trimmed = input.clone().>strip()
    
    if trimmed == "" then
      prompt()
    elseif trimmed == "/q" then
      _env.out.print("Goodbye!")
    elseif trimmed == "/c" then
      _messages = Array[Message]
      _env.out.print(Colors.g() + "⏺ Cleared" + Colors.r())
      prompt()
    else
      _messages.push(Message("user", consume trimmed))
      
      // Response
      _env.out.print("")
      _env.out.print(Colors.c() + "⏺" + Colors.r() + " Pony actors provide isolation!")
      _env.out.print(Colors.d() + "  Each tool runs in its own actor!" + Colors.r())
      _env.out.print("")
      
      prompt()
    end

// Input notifier
class Notify is InputNotify
  let _main: Main
  
  new iso create(main: Main) =>
    _main = main
  
  fun ref apply(data: Array[U8] iso) =>
    _main.handle_input(String.from_array(consume data))

// Why Pony for AI agents?
// • Actors = isolated tool execution
// • Capabilities = secure by design
// • No data races = parallel safety
// • Reference capabilities = memory safe
//
// Perfect for multi-agent systems!
