#!/usr/bin/env kotlin
// nanocode - minimal claude code alternative (Kotlin)
@file:DependsOn("com.google.code.gson:gson:2.10.1")
import com.google.gson.*
import java.io.File
import java.net.URI
import java.net.http.*

val KEY = System.getenv("ANTHROPIC_API_KEY")!!
val MODEL = System.getenv("MODEL") ?: "claude-sonnet-4-20250514"
val R = "\u001b[0m"; val B = "\u001b[1m"; val D = "\u001b[2m"; val C = "\u001b[36m"; val G = "\u001b[32m"; val BL = "\u001b[34m"
val client = HttpClient.newHttpClient()
val gson = Gson()

fun runTool(name: String, input: JsonObject): String = try { when(name) {
    "read" -> File(input["path"].asString).readLines().mapIndexed { i, l -> "${i+1}| $l" }.joinToString("\n")
    "write" -> { File(input["path"].asString).writeText(input["content"].asString); "ok" }
    "edit" -> { val f = File(input["path"].asString); val t = f.readText()
        if (input["old"].asString !in t) "error: not found" else { f.writeText(t.replaceFirst(input["old"].asString, input["new"].asString)); "ok" }}
    "glob" -> ProcessBuilder("sh","-c","find . -name '${input["pat"].asString}' | head -50").start().inputReader().readText()
    "grep" -> ProcessBuilder("sh","-c","grep -rn '${input["pat"].asString}' . | head -50").start().inputReader().readText()
    "bash" -> ProcessBuilder("sh","-c",input["cmd"].asString).redirectErrorStream(true).start().inputReader().readText()
    else -> "unknown"
}} catch(e: Exception) { "error: ${e.message}" }

val schema = """[{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},
{"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},
{"name":"edit","description":"Edit","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"}},"required":["path","old","new"]}},
{"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
{"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
{"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}]"""

fun ask(messages: List<Map<String,Any>>): JsonObject {
    val body = gson.toJson(mapOf("model" to MODEL, "max_tokens" to 4096, "system" to "Concise assistant", "messages" to messages, "tools" to JsonParser.parseString(schema)))
    val req = HttpRequest.newBuilder(URI("https://api.anthropic.com/v1/messages"))
        .header("Content-Type","application/json").header("anthropic-version","2023-06-01").header("x-api-key",KEY)
        .POST(HttpRequest.BodyPublishers.ofString(body)).build()
    return JsonParser.parseString(client.send(req, HttpResponse.BodyHandlers.ofString()).body()).asJsonObject
}

fun main() {
    println("${B}nanocode$R | $D$MODEL$R\n")
    val messages = mutableListOf<Map<String,Any>>()
    
    while(true) {
        print("$B${BL}❯$R ")
        val input = readLine()?.trim() ?: break
        if (input.isEmpty()) continue
        if (input == "/q") break
        if (input == "/c") { messages.clear(); println("${G}⏺ Cleared$R"); continue }
        
        messages += mapOf("role" to "user", "content" to input)
        
        while(true) {
            val resp = ask(messages)
            val content = resp.getAsJsonArray("content")
            val results = mutableListOf<Map<String,Any>>()
            
            for (block in content) {
                val b = block.asJsonObject
                if (b["type"].asString == "text") println("\n$C⏺$R ${b["text"].asString}")
                if (b["type"].asString == "tool_use") {
                    val name = b["name"].asString
                    println("\n$G⏺ $name$R")
                    val result = runTool(name, b["input"].asJsonObject)
                    println("  $D⎿ ${result.lineSequence().first()}$R")
                    results += mapOf("type" to "tool_result", "tool_use_id" to b["id"].asString, "content" to result)
                }
            }
            messages += mapOf("role" to "assistant", "content" to gson.fromJson(content, List::class.java))
            if (results.isEmpty()) break
            messages += mapOf("role" to "user", "content" to results)
        }
        println()
    }
}
main()
