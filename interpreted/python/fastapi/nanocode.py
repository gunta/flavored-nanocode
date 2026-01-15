#!/usr/bin/env python3
"""nanocode - minimal claude code alternative (FastAPI web UI)"""
# pip install fastapi uvicorn && uvicorn nanocode:app
from fastapi import FastAPI
from fastapi.responses import HTMLResponse
from pydantic import BaseModel
import glob, json, os, re, subprocess, urllib.request

KEY = os.environ.get("ANTHROPIC_API_KEY")
MODEL = os.environ.get("MODEL", "claude-opus-4-5")
messages = []


def _read(a):
    lines = open(a["path"]).read().splitlines()
    off = a.get("offset", 0)
    lim = a.get("limit", len(lines))
    return "\n".join(f"{off+i+1}| {l}" for i, l in enumerate(lines[off : off + lim]))


def _write(a):
    open(a["path"], "w").write(a["content"])
    return "ok"


def _edit(a):
    text = open(a["path"]).read()
    old, new = a["old"], a["new"]
    count = text.count(old)
    if not count:
        return "error: old_string not found"
    if not a.get("all") and count > 1:
        return f"error: old_string appears {count} times, use all=true"
    updated = text.replace(old, new) if a.get("all") else text.replace(old, new, 1)
    open(a["path"], "w").write(updated)
    return "ok"


def _glob(a):
    pat = (a.get("path", ".") + "/" + a["pat"]).replace("//", "/")
    files = sorted(glob.glob(pat, recursive=True))
    return "\n".join(files) or "none"


def _grep(a):
    regex = re.compile(a["pat"])
    hits = []
    for path in glob.glob(a.get("path", ".") + "/**", recursive=True):
        if os.path.isdir(path):
            continue
        try:
            for i, line in enumerate(open(path), 1):
                if regex.search(line):
                    hits.append(f"{path}:{i}:{line.rstrip()}")
        except Exception:
            continue
    return "\n".join(hits[:50]) or "none"


def _bash(a):
    run = subprocess.run(a["cmd"], shell=True, capture_output=True, text=True)
    return (run.stdout + run.stderr).strip() or "(empty)"


tools = {"read": _read, "write": _write, "edit": _edit, "glob": _glob, "grep": _grep, "bash": _bash}

schema = [
    {"name": "read", "description": "Read file", "input_schema": {"type": "object", "properties": {"path": {"type": "string"}, "offset": {"type": "integer"}, "limit": {"type": "integer"}}, "required": ["path"]}},
    {"name": "write", "description": "Write file", "input_schema": {"type": "object", "properties": {"path": {"type": "string"}, "content": {"type": "string"}}, "required": ["path", "content"]}},
    {"name": "edit", "description": "Replace text", "input_schema": {"type": "object", "properties": {"path": {"type": "string"}, "old": {"type": "string"}, "new": {"type": "string"}, "all": {"type": "boolean"}}, "required": ["path", "old", "new"]}},
    {"name": "glob", "description": "Find files", "input_schema": {"type": "object", "properties": {"pat": {"type": "string"}, "path": {"type": "string"}}, "required": ["pat"]}},
    {"name": "grep", "description": "Search files", "input_schema": {"type": "object", "properties": {"pat": {"type": "string"}, "path": {"type": "string"}}, "required": ["pat"]}},
    {"name": "bash", "description": "Run command", "input_schema": {"type": "object", "properties": {"cmd": {"type": "string"}}, "required": ["cmd"]}},
]

def ask():
    req = urllib.request.Request("https://api.anthropic.com/v1/messages",
        data=json.dumps({"model": MODEL, "max_tokens": 4096, "system": "Concise assistant", "messages": messages, "tools": schema}).encode(),
        headers={"Content-Type": "application/json", "anthropic-version": "2023-06-01", "x-api-key": KEY})
    return json.loads(urllib.request.urlopen(req).read())

app = FastAPI()

class Msg(BaseModel):
    msg: str

@app.get("/", response_class=HTMLResponse)
def index():
    return """<!DOCTYPE html><html><head><title>nanocode</title>
<style>body{font:14px monospace;background:#0d1117;color:#c9d1d9;max-width:800px;margin:0 auto;padding:20px}
#out{white-space:pre-wrap}form{display:flex}input{flex:1;background:#21262d;border:1px solid #30363d;color:#c9d1d9;padding:8px}
button{background:#238636;border:none;color:#fff;padding:8px 16px}</style></head>
<body><h2>🧬 nanocode (FastAPI)</h2><div id="out"></div>
<form onsubmit="send(event)"><input id="i" autofocus><button>→</button></form>
<script>async function send(e){e.preventDefault();const m=i.value;i.value='';out.innerHTML+='❯ '+m+'\\n';
const r=await(await fetch('/chat',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({msg:m})})).json();
out.innerHTML+=r.map(b=>b.type==='text'?'⏺ '+b.text:'⚙ '+b.name).join('\\n')+'\\n\\n';}</script></body></html>"""

@app.post("/chat")
def chat(body: Msg):
    global messages
    if body.msg == "/c": messages = []; return [{"type": "text", "text": "Cleared"}]
    messages.append({"role": "user", "content": body.msg})
    out = []
    while True:
        content = ask().get("content", [])
        for block in content:
            if block["type"] == "text": out.append({"type": "text", "text": block["text"]})
            if block["type"] == "tool_use":
                out.append({"type": "tool", "name": block["name"]})
                result = tools[block["name"]](block["input"])
                messages.extend([{"role": "assistant", "content": content}, {"role": "user", "content": [{"type": "tool_result", "tool_use_id": block["id"], "content": result}]}])
        if not any(b["type"] == "tool_use" for b in content): messages.append({"role": "assistant", "content": content}); break
    return out
