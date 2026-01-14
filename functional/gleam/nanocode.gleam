// nanocode - minimal claude code alternative (Gleam)
// gleam run
import gleam/io
import gleam/string
import gleam/list
import gleam/result
import gleam/json
import gleam/httpc
import gleam/erlang/os
import gleam/erlang/process
import simplifile

const r = "\u{001b}[0m"
const b = "\u{001b}[1m"
const d = "\u{001b}[2m"
const c = "\u{001b}[36m"
const g = "\u{001b}[32m"
const bl = "\u{001b}[34m"

fn tool(name: String, input: json.Json) -> String {
  case name {
    "read" -> {
      let path = json.get_string(input, "path") |> result.unwrap("")
      case simplifile.read(path) {
        Ok(content) -> content |> string.split("\n") |> list.index_map(fn(l, i) { int.to_string(i + 1) <> "| " <> l }) |> string.join("\n")
        Error(_) -> "error: file not found"
      }
    }
    "write" -> {
      let path = json.get_string(input, "path") |> result.unwrap("")
      let content = json.get_string(input, "content") |> result.unwrap("")
      case simplifile.write(path, content) { Ok(_) -> "ok" Error(_) -> "error" }
    }
    "bash" -> {
      let cmd = json.get_string(input, "cmd") |> result.unwrap("")
      os.cmd("sh", ["-c", cmd]) |> result.unwrap("")
    }
    _ -> "unknown"
  }
}

pub fn main() {
  let key = os.get_env("ANTHROPIC_API_KEY") |> result.unwrap("")
  let model = os.get_env("MODEL") |> result.unwrap("claude-sonnet-4-20250514")
  
  io.println(b <> "nanocode" <> r <> " | " <> d <> model <> r <> "\n")
  loop(key, model, [])
}

fn loop(key: String, model: String, messages: List(json.Json)) {
  io.print(b <> bl <> "❯" <> r <> " ")
  case erlang.get_line("") {
    Error(_) -> Nil
    Ok(input) -> {
      let input = string.trim(input)
      case input {
        "" -> loop(key, model, messages)
        "/q" -> Nil
        "/c" -> { io.println(g <> "⏺ Cleared" <> r); loop(key, model, []) }
        _ -> {
          let messages = list.append(messages, [json.object([#("role", json.string("user")), #("content", json.string(input))])])
          let messages = agent_loop(key, model, messages)
          io.println("")
          loop(key, model, messages)
        }
      }
    }
  }
}

fn agent_loop(key: String, model: String, messages: List(json.Json)) -> List(json.Json) {
  // API call and tool handling would go here
  // Gleam's HTTP client setup is more verbose, simplified for demo
  messages
}
