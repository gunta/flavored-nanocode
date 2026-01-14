#!/usr/bin/env groovy
// nanocode - minimal claude code alternative (Groovy)
@Grab('com.google.code.gson:gson:2.10.1')
import com.google.gson.*
import groovy.json.*

def KEY = System.getenv('ANTHROPIC_API_KEY')
def MODEL = System.getenv('MODEL') ?: 'claude-sonnet-4-20250514'
def (R,B,D,C,G,BL) = ['\033[0m','\033[1m','\033[2m','\033[36m','\033[32m','\033[34m']

def tools = [
    read: { a -> new File(a.path).readLines().withIndex().collect { l,i -> "${i+1}| $l" }.join('\n') },
    write: { a -> new File(a.path).text = a.content; 'ok' },
    edit: { a -> def f = new File(a.path), t = f.text; t.contains(a.old) ? (f.text = t.replaceFirst(a.old, a.new); 'ok') : 'error: not found' },
    glob: { a -> "find . -name '${a.pat}'".execute().text.readLines().take(50).join('\n') ?: 'none' },
    grep: { a -> "grep -rn '${a.pat}' .".execute().text.readLines().take(50).join('\n') ?: 'none' },
    bash: { a -> ['sh','-c',a.cmd].execute().text }
]

def schema = '''[{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},
{"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},
{"name":"edit","description":"Edit","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"}},"required":["path","old","new"]}},
{"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
{"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
{"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}]'''

def ask = { messages ->
    def conn = new URL('https://api.anthropic.com/v1/messages').openConnection()
    conn.requestMethod = 'POST'
    conn.setRequestProperty('Content-Type', 'application/json')
    conn.setRequestProperty('anthropic-version', '2023-06-01')
    conn.setRequestProperty('x-api-key', KEY)
    conn.doOutput = true
    conn.outputStream.withWriter { it << JsonOutput.toJson([model: MODEL, max_tokens: 4096, system: 'Concise assistant', messages: messages, tools: new JsonSlurper().parseText(schema)]) }
    new JsonSlurper().parseText(conn.inputStream.text)
}

println "${B}nanocode${R} | ${D}${MODEL}${R}\n"
def messages = []

System.in.newReader().eachLine { input ->
    print "${B}${BL}❯${R} "
    if (!input?.trim()) return
    if (input == '/q') System.exit(0)
    if (input == '/c') { messages = []; println "${G}⏺ Cleared${R}"; return }
    
    messages << [role: 'user', content: input]
    
    while (true) {
        def resp = ask(messages)
        def content = resp.content ?: []
        def results = []
        
        content.each { block ->
            if (block.type == 'text') println "\n${C}⏺${R} ${block.text}"
            if (block.type == 'tool_use') {
                println "\n${G}⏺ ${block.name}${R}"
                def result = tools[block.name](block.input)
                println "  ${D}⎿ ${result.readLines()[0]}${R}"
                results << [type: 'tool_result', tool_use_id: block.id, content: result]
            }
        }
        messages << [role: 'assistant', content: content]
        if (!results) break
        messages << [role: 'user', content: results]
    }
    println()
}
