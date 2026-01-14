#!/usr/bin/env python3
"""nanocode - minimal claude code alternative (~150 lines, zero deps)"""
import glob as G, json, os, re, subprocess, urllib.request

KEY = os.environ.get("OPENROUTER_API_KEY") or os.environ.get("ANTHROPIC_API_KEY")
API = "https://openrouter.ai/api/v1/messages" if os.environ.get("OPENROUTER_API_KEY") else "https://api.anthropic.com/v1/messages"
MODEL = os.environ.get("MODEL", "anthropic/claude-opus-4" if os.environ.get("OPENROUTER_API_KEY") else "claude-sonnet-4-20250514")
R, B, D, C, GR, BL = "\033[0m", "\033[1m", "\033[2m", "\033[36m", "\033[32m", "\033[34m"

tools = {
    "read": lambda a: "\n".join(f"{i+1}| {l}" for i, l in enumerate(open(a["path"]).read().split("\n")[a.get("offset", 0):a.get("offset", 0) + a.get("limit", 9999)])),
    "write": lambda a: (open(a["path"], "w").write(a["content"]), "ok")[1],
    "edit": lambda a: (lambda t: "error: not found" if a["old"] not in t else (f"error: {t.count(a['old'])}x, use all=true" if not a.get("all") and t.count(a["old"]) > 1 else (open(a["path"], "w").write(t.replace(a["old"], a["new"]) if a.get("all") else t.replace(a["old"], a["new"], 1)), "ok")[1]))(open(a["path"]).read()),
    "glob": lambda a: "\n".join(sorted(G.glob((a.get("path", ".") + "/" + a["pat"]).replace("//", "/"), recursive=True), key=lambda f: os.path.getmtime(f) if os.path.isfile(f) else 0, reverse=True)[:50]) or "none",
    "grep": lambda a: "\n".join(f"{f}:{n}:{l.rstrip()}" for f in G.glob(a.get("path", ".") + "/**", recursive=True) for n, l in enumerate(open(f, errors="ignore"), 1) if re.search(a["pat"], l))[:50] or "none",
    "bash": lambda a: subprocess.run(a["cmd"], shell=True, capture_output=True, text=True, timeout=30).stdout or subprocess.run(a["cmd"], shell=True, capture_output=True, text=True).stderr or "(empty)",
}

schema = [
    {"name": "read", "description": "Read file", "input_schema": {"type": "object", "properties": {"path": {"type": "string"}, "offset": {"type": "integer"}, "limit": {"type": "integer"}}, "required": ["path"]}},
    {"name": "write", "description": "Write file", "input_schema": {"type": "object", "properties": {"path": {"type": "string"}, "content": {"type": "string"}}, "required": ["path", "content"]}},
    {"name": "edit", "description": "Edit file", "input_schema": {"type": "object", "properties": {"path": {"type": "string"}, "old": {"type": "string"}, "new": {"type": "string"}, "all": {"type": "boolean"}}, "required": ["path", "old", "new"]}},
    {"name": "glob", "description": "Find files", "input_schema": {"type": "object", "properties": {"pat": {"type": "string"}, "path": {"type": "string"}}, "required": ["pat"]}},
    {"name": "grep", "description": "Search files", "input_schema": {"type": "object", "properties": {"pat": {"type": "string"}, "path": {"type": "string"}}, "required": ["pat"]}},
    {"name": "bash", "description": "Run command", "input_schema": {"type": "object", "properties": {"cmd": {"type": "string"}}, "required": ["cmd"]}},
]

def ask(messages):
    req = urllib.request.Request(API, json.dumps({"model": MODEL, "max_tokens": 8192, "system": f"Concise coding assistant. cwd: {os.getcwd()}", "messages": messages, "tools": schema}).encode(),
        {"Content-Type": "application/json", "anthropic-version": "2023-06-01", **({"Authorization": f"Bearer {KEY}"} if os.environ.get("OPENROUTER_API_KEY") else {"x-api-key": KEY})})
    return json.loads(urllib.request.urlopen(req).read())

def main():
    print(f"{B}nanocode{R} | {D}{MODEL} | {os.getcwd()}{R}\n")
    messages = []
    while True:
        try:
            user_input = input(f"{B}{BL}❯{R} ").strip()
            if not user_input: continue
            if user_input in ("/q", "exit"): break
            if user_input == "/c": messages = []; print(f"{GR}⏺ Cleared{R}"); continue

            messages.append({"role": "user", "content": user_input})
            while True:
                resp = ask(messages)
                content = resp.get("content", [])
                results = []
                for block in content:
                    if block["type"] == "text": print(f"\n{C}⏺{R} {block['text']}")
                    if block["type"] == "tool_use":
                        print(f"\n{GR}⏺ {block['name']}{R}({D}{str(list(block['input'].values())[0])[:50]}{R})")
                        try: result = tools[block["name"]](block["input"])
                        except Exception as e: result = f"error: {e}"
                        print(f"  {D}⎿ {result.split(chr(10))[0][:60]}{R}")
                        results.append({"type": "tool_result", "tool_use_id": block["id"], "content": result})
                messages.append({"role": "assistant", "content": content})
                if not results: break
                messages.append({"role": "user", "content": results})
            print()
        except (KeyboardInterrupt, EOFError): break
        except Exception as e: print(f"\033[31m⏺ Error: {e}{R}")

if __name__ == "__main__": main()
