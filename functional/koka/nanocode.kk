// nanocode - minimal claude code alternative (Koka)
// koka -e nanocode.kk
// Koka is a functional language with algebraic effects (2012+)

import std/os/env
import std/os/readline
import std/os/path
import std/os/file

// ANSI colors
val r = "\x1b[0m"
val b = "\x1b[1m"
val d = "\x1b[2m"
val c = "\x1b[36m"
val g = "\x1b[32m"
val bl = "\x1b[34m"

// Tool effect - algebraic effects for tool execution!
effect tool
  ctl read-file(path: string): string
  ctl write-file(path: string, content: string): string
  ctl run-bash(cmd: string): string

// Tool handler
fun with-tools(action: () -> <tool,io> a): io a
  with handler
    ctl read-file(path)
      val content = read-text-file(path.path)
      val lines = content.lines.map-indexed fn(i, l) (i + 1).show ++ "| " ++ l
      resume(lines.join("\n"))
    ctl write-file(path, content)
      write-text-file(path.path, content)
      resume("ok")
    ctl run-bash(cmd)
      // Would execute command
      resume("output")
  action()

// Message type
struct message
  role: string
  content: string

// Schema
val schema = """
[{"name":"read","description":"Read file"},
{"name":"write","description":"Write file"},
{"name":"bash","description":"Run command"}]
"""

// Main loop with effects
fun main-loop(messages: list<message>): <io,tool> ()
  print(b ++ bl ++ "❯" ++ r ++ " ")
  match readline()
    Nothing -> ()
    Just(input) ->
      val input' = input.trim
      if input' == "" then main-loop(messages)
      elif input' == "/q" then ()
      elif input' == "/c" then
        println(g ++ "⏺ Cleared" ++ r)
        main-loop([])
      else
        val msgs = messages ++ [Message("user", input')]
        
        // Would call API here
        println("")
        println(c ++ "⏺" ++ r ++ " I'm your Koka-powered assistant!")
        println(d ++ "  Using algebraic effects for tool execution!" ++ r)
        println("")
        
        main-loop(msgs)

// Entry point
pub fun main(): io ()
  val model = get-env("MODEL").default("claude-sonnet-4-20250514")
  println(b ++ "nanocode" ++ r ++ " | " ++ d ++ "Koka + " ++ model ++ r ++ "\n")
  
  with-tools
    main-loop([])
