// nanocode - minimal claude code alternative (MoonBit)
// moon run nanocode.mbt
// MoonBit is a new language (2023+) designed for cloud and edge computing

// Note: MoonBit's stdlib is still evolving, this is a conceptual implementation
fn main {
  let key = @env.get("ANTHROPIC_API_KEY").unwrap_or("")
  let model = @env.get("MODEL").unwrap_or("claude-sonnet-4-20250514")
  
  println("\x1b[1mnanocode\x1b[0m | \x1b[2mMoonBit + \(model)\x1b[0m\n")
  
  let messages : Array[Message] = []
  
  loop {
    print("\x1b[1m\x1b[34m❯\x1b[0m ")
    match @io.read_line() {
      None => break
      Some(input) => {
        let input = input.trim()
        if input.is_empty() { continue }
        if input == "/q" { break }
        if input == "/c" {
          messages.clear()
          println("\x1b[32m⏺ Cleared\x1b[0m")
          continue
        }
        
        messages.push({ role: "user", content: input })
        
        // Agent loop
        loop {
          let resp = ask(key, model, messages)
          let content = resp.content
          let results : Array[ToolResult] = []
          
          for block in content {
            match block {
              Text(text) => println("\n\x1b[36m⏺\x1b[0m \(text)")
              ToolUse(name, id, input) => {
                println("\n\x1b[32m⏺ \(name)\x1b[0m")
                let result = run_tool(name, input)
                println("  \x1b[2m⎿ \(result.split("\n")[0])\x1b[0m")
                results.push({ type_: "tool_result", tool_use_id: id, content: result })
              }
            }
          }
          
          messages.push({ role: "assistant", content: content })
          if results.is_empty() { break }
          messages.push({ role: "user", content: ToolResults(results) })
        }
        println()
      }
    }
  }
}

fn run_tool(name : String, input : Json) -> String {
  match name {
    "read" => {
      let path = input["path"].as_string().unwrap_or("")
      @fs.read_to_string(path)
        .map(fn(content) {
          content.split("\n")
            .enumerate()
            .map(fn((i, l)) { "\(i + 1)| \(l)" })
            .join("\n")
        })
        .unwrap_or("error: file not found")
    }
    "write" => {
      let path = input["path"].as_string().unwrap_or("")
      let content = input["content"].as_string().unwrap_or("")
      @fs.write_string(path, content).map(fn(_) { "ok" }).unwrap_or("error")
    }
    "bash" => {
      let cmd = input["cmd"].as_string().unwrap_or("")
      @process.exec("sh", ["-c", cmd]).stdout
    }
    _ => "unknown"
  }
}

struct Message {
  role : String
  content : Content
}

enum Content {
  Text(String)
  ToolUse(String, String, Json)
  ToolResults(Array[ToolResult])
}

struct ToolResult {
  type_ : String
  tool_use_id : String
  content : String
}

fn ask(key : String, model : String, messages : Array[Message]) -> Response {
  // HTTP request implementation
  @http.post("https://api.anthropic.com/v1/messages")
    .header("Content-Type", "application/json")
    .header("anthropic-version", "2023-06-01")
    .header("x-api-key", key)
    .json({ model, max_tokens: 4096, system: "Concise assistant", messages, tools: schema() })
    .send()
    .json()
}

fn schema() -> Array[Tool] {
  [
    { name: "read", description: "Read file", input_schema: { type_: "object", properties: { path: { type_: "string" } }, required: ["path"] } },
    { name: "write", description: "Write file", input_schema: { type_: "object", properties: { path: { type_: "string" }, content: { type_: "string" } }, required: ["path", "content"] } },
    { name: "bash", description: "Run command", input_schema: { type_: "object", properties: { cmd: { type_: "string" } }, required: ["cmd"] } },
  ]
}
