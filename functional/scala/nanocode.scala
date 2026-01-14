//> using scala 3.3
//> using dep com.lihaoyi::requests:0.8.0
//> using dep com.lihaoyi::ujson:3.1.3
// nanocode - minimal claude code alternative (Scala 3)
import scala.io.StdIn, scala.util.*, java.io.File, scala.sys.process.*

val KEY = sys.env("ANTHROPIC_API_KEY")
val MODEL = sys.env.getOrElse("MODEL", "claude-sonnet-4-20250514")
val (R,B,D,C,G,BL) = ("\u001b[0m","\u001b[1m","\u001b[2m","\u001b[36m","\u001b[32m","\u001b[34m")

def runTool(name: String, input: ujson.Value): String = Try(name match
  case "read" => scala.io.Source.fromFile(input("path").str).getLines.zipWithIndex.map((l,i) => s"${i+1}| $l").mkString("\n")
  case "write" => java.nio.file.Files.writeString(java.nio.file.Path.of(input("path").str), input("content").str); "ok"
  case "edit" => val f = new File(input("path").str); val t = scala.io.Source.fromFile(f).mkString
    if !t.contains(input("old").str) then "error: not found" else { java.nio.file.Files.writeString(f.toPath, t.replaceFirst(java.util.regex.Pattern.quote(input("old").str), input("new").str)); "ok" }
  case "glob" => s"find . -name '${input("pat").str}'".!!.linesIterator.take(50).mkString("\n")
  case "grep" => s"grep -rn '${input("pat").str}' .".!!.linesIterator.take(50).mkString("\n")
  case "bash" => input("cmd").str.!!
).getOrElse("error")

val schema = ujson.read("""[{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},
{"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},
{"name":"edit","description":"Edit","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"}},"required":["path","old","new"]}},
{"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
{"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
{"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}]""")

def ask(messages: ujson.Arr) = ujson.read(requests.post("https://api.anthropic.com/v1/messages",
  headers = Map("Content-Type" -> "application/json", "anthropic-version" -> "2023-06-01", "x-api-key" -> KEY),
  data = ujson.Obj("model" -> MODEL, "max_tokens" -> 4096, "system" -> "Concise assistant", "messages" -> messages, "tools" -> schema)).text())

@main def main() =
  println(s"${B}nanocode$R | $D$MODEL$R\n")
  var messages = ujson.Arr()
  
  while true do
    print(s"$B${BL}❯$R ")
    Option(StdIn.readLine()).map(_.trim) match
      case None | Some("/q") => return
      case Some("") => ()
      case Some("/c") => messages = ujson.Arr(); println(s"${G}⏺ Cleared$R")
      case Some(input) =>
        messages.value += ujson.Obj("role" -> "user", "content" -> input)
        var continue = true
        while continue do
          val resp = ask(messages)
          val content = resp("content").arr
          val results = ujson.Arr()
          content.foreach { block =>
            if block("type").str == "text" then println(s"\n$C⏺$R ${block("text").str}")
            if block("type").str == "tool_use" then
              val name = block("name").str
              println(s"\n$G⏺ $name$R")
              val result = runTool(name, block("input"))
              println(s"  $D⎿ ${result.linesIterator.nextOption.getOrElse("")}$R")
              results.value += ujson.Obj("type" -> "tool_result", "tool_use_id" -> block("id"), "content" -> result)
          }
          messages.value += ujson.Obj("role" -> "assistant", "content" -> content)
          if results.value.isEmpty then continue = false else messages.value += ujson.Obj("role" -> "user", "content" -> results)
        println()
