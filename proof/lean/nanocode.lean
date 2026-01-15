-- nanocode - minimal claude code alternative (Lean 4)
-- lake run nanocode
-- Lean is a theorem prover AND programming language

import Lean
import Std

open IO System

def R := "\x1b[0m"
def B := "\x1b[1m"
def D := "\x1b[2m"
def C := "\x1b[36m"
def G := "\x1b[32m"
def BL := "\x1b[34m"

structure ToolInput where
  path : Option String := none
  content : Option String := none
  cmd : Option String := none
  pat : Option String := none
  old : Option String := none
  new : Option String := none
  all : Bool := false
  offset : Option Nat := none
  limit : Option Nat := none

partial def tool (name : String) (input : ToolInput) : IO String := do
  match name with
  | "read" =>
    match input.path with
    | some p =>
      let content ← FS.readFile p
      let lines := content.splitOn "\n"
      let off := input.offset.getD 0
      let lim := input.limit.getD lines.length
      let slice := (lines.drop off).take lim
      let numbered := slice.enum.map fun (i, l) => s!"{off + i + 1}| {l}"
      return "\n".intercalate numbered
    | none => return "error: no path"
  | "write" =>
    match input.path, input.content with
    | some p, some c => do FS.writeFile p c; return "ok"
    | _, _ => return "error"
  | "edit" =>
    match input.path, input.old, input.new with
    | some p, some o, some n =>
      let txt ← FS.readFile p
      let cnt := txt.countSubstr o
      if cnt == 0 then
        return "error: old_string not found"
      else if cnt > 1 ∧ ¬ input.all then
        return s!"error: old_string appears {cnt} times, use all=true"
      else
        let replaced := txt.replace o n
        FS.writeFile p replaced
        return "ok"
    | _, _, _ => return "error"
  | "bash" =>
    match input.cmd with
    | some c =>
      let out ← Process.run { cmd := "sh", args := #["-c", c] }
      return out.stdout
    | none => return "error"
  | "glob" =>
    match input.pat with
    | some p =>
      let out ← Process.run { cmd := "find", args := #[".", "-name", p] }
      return out.stdout
    | none => return "none"
  | "grep" =>
    match input.pat with
    | some p =>
      let out ← Process.run { cmd := "grep", args := #["-R", p, ".", "-n"] }
      return out.stdout
    | none => return "none"
  | _ => return "unknown"

def schema : List Json := [
  .mkObj [("name", "read"), ("description", "Read file"), ("input_schema", .mkObj [("type", "object"), ("properties", .mkObj [("path", .mkObj [("type", "string")]), ("offset", .mkObj [("type", "integer")]), ("limit", .mkObj [("type", "integer")])]), ("required", .arr #["path"])])],
  .mkObj [("name", "write"), ("description", "Write file"), ("input_schema", .mkObj [("type", "object"), ("properties", .mkObj [("path", .mkObj [("type", "string")]), ("content", .mkObj [("type", "string")])]), ("required", .arr #["path", "content"])])],
  .mkObj [("name", "edit"), ("description", "Replace text"), ("input_schema", .mkObj [("type", "object"), ("properties", .mkObj [("path", .mkObj [("type", "string")]), ("old", .mkObj [("type", "string")]), ("new", .mkObj [("type", "string")]), ("all", .mkObj [("type", "boolean")])]), ("required", .arr #["path", "old", "new"])])],
  .mkObj [("name", "glob"), ("description", "Find files"), ("input_schema", .mkObj [("type", "object"), ("properties", .mkObj [("pat", .mkObj [("type", "string")])]), ("required", .arr #["pat"])])],
  .mkObj [("name", "grep"), ("description", "Search files"), ("input_schema", .mkObj [("type", "object"), ("properties", .mkObj [("pat", .mkObj [("type", "string")])]), ("required", .arr #["pat"])])],
  .mkObj [("name", "bash"), ("description", "Run command"), ("input_schema", .mkObj [("type", "object"), ("properties", .mkObj [("cmd", .mkObj [("type", "string")])]), ("required", .arr #["cmd"])])]
]

def ask (key model : String) (messages : List Json) : IO Json := do
  let body := Json.mkObj [
    ("model", model),
    ("max_tokens", 4096),
    ("system", "Concise coding assistant"),
    ("messages", .arr messages.toArray),
    ("tools", .arr schema.toArray)
  ]
  -- HTTP call would go here
  return .null

def main : IO Unit := do
  let _ ← getEnv "ANTHROPIC_API_KEY"
  let model ← (getEnv "MODEL").map (·.getD "claude-opus-4-5")
  println! "{B}nanocode{R} | {D}Lean 4 + {model}{R}\n"
  let mut messages : List Json := []
  repeat do
    print! "{B}{BL}❯{R} "
    let (stdin, _) ← getStdin
    let input ← stdin.getLine
    let input := input.trim
    if input.isEmpty then continue
    if input == "/q" then break
    if input == "/c" then
      messages := []
      println! "{G}⏺ Cleared{R}"
      continue
    messages := messages ++ [.mkObj [("role", "user"), ("content", input)]]
    repeat do
      let _resp ← ask "" model messages
      break
    println! ""
