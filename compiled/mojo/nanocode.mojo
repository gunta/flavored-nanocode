# nanocode - minimal claude code alternative (Mojo 🔥)
# mojo run nanocode.mojo
from python import Python
from sys import env

fn main() raises:
    let os = Python.import_module("os")
    let json = Python.import_module("json")
    let urllib = Python.import_module("urllib.request")
    
    let KEY = str(os.environ.get("ANTHROPIC_API_KEY", ""))
    let MODEL = str(os.environ.get("MODEL", "claude-sonnet-4-20250514"))
    
    let R = "\033[0m"
    let B = "\033[1m"
    let D = "\033[2m"
    let C = "\033[36m"
    let G = "\033[32m"
    let BL = "\033[34m"
    
    print(B + "nanocode" + R + " | " + D + "Mojo 🔥 + " + MODEL + R + "\n")
    
    var messages = Python.list()
    let schema = json.loads("""[
        {"name":"read","description":"Read file","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},
        {"name":"write","description":"Write file","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},
        {"name":"bash","description":"Run command","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}
    ]""")
    
    while True:
        let input_str = input(B + BL + "❯" + R + " ")
        if len(input_str) == 0:
            continue
        if input_str == "/q":
            break
        if input_str == "/c":
            messages = Python.list()
            print(G + "⏺ Cleared" + R)
            continue
        
        messages.append({"role": "user", "content": input_str})
        
        # API call using Python interop
        while True:
            let body = json.dumps({
                "model": MODEL,
                "max_tokens": 4096,
                "system": "Concise coding assistant",
                "messages": messages,
                "tools": schema
            }).encode()
            
            let req = urllib.Request(
                "https://api.anthropic.com/v1/messages",
                data=body,
                headers={
                    "Content-Type": "application/json",
                    "anthropic-version": "2023-06-01",
                    "x-api-key": KEY
                }
            )
            
            let resp = json.loads(urllib.urlopen(req).read())
            let content = resp.get("content", [])
            var results = Python.list()
            
            for block in content:
                if block["type"] == "text":
                    print("\n" + C + "⏺" + R + " " + str(block["text"]))
                if block["type"] == "tool_use":
                    let name = str(block["name"])
                    print("\n" + G + "⏺ " + name + R)
                    var result = "ok"
                    # Tool execution via Python
                    if name == "read":
                        try:
                            with open(str(block["input"]["path"]), "r") as f:
                                let lines = f.read().split("\n")
                                result = "\n".join([str(i+1) + "| " + l for i, l in enumerate(lines)])
                        except:
                            result = "error: file not found"
                    elif name == "bash":
                        let subprocess = Python.import_module("subprocess")
                        result = str(subprocess.run(str(block["input"]["cmd"]), shell=True, capture_output=True, text=True).stdout)
                    print("  " + D + "⎿ " + result.split("\n")[0][:60] + R)
                    results.append({"type": "tool_result", "tool_use_id": block["id"], "content": result})
            
            messages.append({"role": "assistant", "content": content})
            if len(results) == 0:
                break
            messages.append({"role": "user", "content": results})
        
        print()
