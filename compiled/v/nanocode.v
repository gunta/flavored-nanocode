// nanocode - minimal claude code alternative (V)
// v run nanocode.v
import os
import net.http
import json
import encoding.utf8

const r = '\e[0m'
const b = '\e[1m'
const d = '\e[2m'
const c = '\e[36m'
const g = '\e[32m'
const bl = '\e[34m'

fn tool(name string, input map[string]json.Any) string {
    match name {
        'read' {
            content := os.read_file(input['path']!.str()) or { return 'error: ${err}' }
            lines := content.split('\n')
            mut result := []string{}
            for i, line in lines {
                result << '${i + 1}| ${line}'
            }
            return result.join('\n')
        }
        'write' {
            os.write_file(input['path']!.str(), input['content']!.str()) or { return 'error: ${err}' }
            return 'ok'
        }
        'bash' {
            res := os.execute(input['cmd']!.str())
            return res.output
        }
        'glob' {
            res := os.execute("find . -name '${input['pat']!.str()}' | head -50")
            return res.output
        }
        'grep' {
            res := os.execute("grep -rn '${input['pat']!.str()}' . | head -50")
            return res.output
        }
        else {
            return 'unknown'
        }
    }
}

fn ask(key string, model string, messages []map[string]json.Any) !map[string]json.Any {
    schema := '[{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},{"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},{"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}},{"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},{"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}}]'
    body := json.encode({
        'model': json.Any(model)
        'max_tokens': json.Any(4096)
        'system': json.Any('Concise assistant')
        'messages': json.Any(messages)
        'tools': json.decode(schema)!
    })
    
    mut req := http.new_request(.post, 'https://api.anthropic.com/v1/messages', body)!
    req.add_header(.content_type, 'application/json')
    req.add_custom_header('anthropic-version', '2023-06-01')!
    req.add_custom_header('x-api-key', key)!
    
    resp := req.do()!
    return json.decode(map[string]json.Any, resp.body)!
}

fn main() {
    key := os.getenv('ANTHROPIC_API_KEY')
    model := os.getenv_opt('MODEL') or { 'claude-sonnet-4-20250514' }
    
    println('${b}nanocode${r} | ${d}${model}${r}\n')
    
    mut messages := []map[string]json.Any{}
    
    for {
        print('${b}${bl}❯${r} ')
        input := os.get_line().trim_space()
        
        if input == '' { continue }
        if input == '/q' { break }
        if input == '/c' {
            messages = []
            println('${g}⏺ Cleared${r}')
            continue
        }
        
        messages << {
            'role': json.Any('user')
            'content': json.Any(input)
        }
        
        for {
            resp := ask(key, model, messages) or {
                println('Error: ${err}')
                break
            }
            
            content := resp['content']!.arr()
            mut results := []map[string]json.Any{}
            
            for block in content {
                block_map := block.as_map()
                if block_map['type']!.str() == 'text' {
                    println('\n${c}⏺${r} ${block_map['text']!.str()}')
                }
                if block_map['type']!.str() == 'tool_use' {
                    name := block_map['name']!.str()
                    println('\n${g}⏺ ${name}${r}')
                    result := tool(name, block_map['input']!.as_map())
                    println('  ${d}⎿ ${result.split('\n')[0]}${r}')
                    results << {
                        'type': json.Any('tool_result')
                        'tool_use_id': block_map['id']!
                        'content': json.Any(result)
                    }
                }
            }
            
            messages << {
                'role': json.Any('assistant')
                'content': json.Any(content)
            }
            
            if results.len == 0 { break }
            messages << {
                'role': json.Any('user')
                'content': json.Any(results)
            }
        }
        println('')
    }
}
