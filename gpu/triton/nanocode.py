#!/usr/bin/env python3
"""nanocode - minimal claude code alternative (Triton GPU)

Triton is OpenAI's GPU programming language for deep learning.
This demonstrates how AI agents could run on GPUs for massive parallelism.

pip install triton && python nanocode.py
"""

import triton
import triton.language as tl
import torch
import json
import os
import glob as globlib
import re
import urllib.request

# GPU-accelerated string processing kernel
@triton.jit
def process_tokens_kernel(
    input_ptr, output_ptr,
    n_elements,
    BLOCK_SIZE: tl.constexpr,
):
    """Process tokens in parallel on GPU"""
    pid = tl.program_id(axis=0)
    block_start = pid * BLOCK_SIZE
    offsets = block_start + tl.arange(0, BLOCK_SIZE)
    mask = offsets < n_elements
    
    # Load input tokens
    x = tl.load(input_ptr + offsets, mask=mask)
    
    # Simple processing (in real impl, would do embedding lookup, etc.)
    output = x
    
    # Store output
    tl.store(output_ptr + offsets, output, mask=mask)

def gpu_process(text: str) -> str:
    """Process text on GPU (demonstration)"""
    # Convert to tensor
    tokens = torch.tensor([ord(c) for c in text], dtype=torch.int32, device='cuda')
    output = torch.empty_like(tokens)
    
    # Launch kernel
    n_elements = tokens.numel()
    grid = lambda meta: (triton.cdiv(n_elements, meta['BLOCK_SIZE']),)
    process_tokens_kernel[grid](tokens, output, n_elements, BLOCK_SIZE=1024)
    
    # Convert back
    return ''.join(chr(c) for c in output.cpu().tolist())

# Standard nanocode implementation
OPENROUTER_KEY = os.environ.get("OPENROUTER_API_KEY")
KEY = OPENROUTER_KEY or os.environ.get("ANTHROPIC_API_KEY")
API = os.environ.get("API_URL", "https://openrouter.ai/api/v1/messages" if OPENROUTER_KEY else "https://api.anthropic.com/v1/messages")
MODEL = os.environ.get("MODEL", "anthropic/claude-opus-4-5" if OPENROUTER_KEY else "claude-opus-4-5")
R, B, D, C, G, BL = "\033[0m", "\033[1m", "\033[2m", "\033[36m", "\033[32m", "\033[34m"


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
    if count > 1 and not a.get("all"):
        return f"error: old_string appears {count} times, use all=true"
    updated = text.replace(old, new) if a.get("all") else text.replace(old, new, 1)
    open(a["path"], "w").write(updated)
    return "ok"


def _glob(a):
    pat = (a.get("path", ".") + "/" + a["pat"]).replace("//", "/")
    files = sorted(globlib.glob(pat, recursive=True))
    return "\n".join(files) or "none"


def _grep(a):
    regex = re.compile(a["pat"])
    hits = []
    for path in globlib.glob(a.get("path", ".") + "/**", recursive=True):
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
    return __import__("subprocess").run(a["cmd"], shell=True, capture_output=True, text=True).stdout


tools = {"read": _read, "write": _write, "edit": _edit, "glob": _glob, "grep": _grep, "bash": _bash}

schema = [
    {"name": "read", "description": "Read file", "input_schema": {"type": "object", "properties": {"path": {"type": "string"}, "offset": {"type": "integer"}, "limit": {"type": "integer"}}, "required": ["path"]}},
    {"name": "write", "description": "Write file", "input_schema": {"type": "object", "properties": {"path": {"type": "string"}, "content": {"type": "string"}}, "required": ["path", "content"]}},
    {"name": "edit", "description": "Replace text", "input_schema": {"type": "object", "properties": {"path": {"type": "string"}, "old": {"type": "string"}, "new": {"type": "string"}, "all": {"type": "boolean"}}, "required": ["path", "old", "new"]}},
    {"name": "glob", "description": "Find files", "input_schema": {"type": "object", "properties": {"pat": {"type": "string"}, "path": {"type": "string"}}, "required": ["pat"]}},
    {"name": "grep", "description": "Search files", "input_schema": {"type": "object", "properties": {"pat": {"type": "string"}, "path": {"type": "string"}}, "required": ["pat"]}},
    {"name": "bash", "description": "Run command", "input_schema": {"type": "object", "properties": {"cmd": {"type": "string"}}, "required": ["cmd"]}},
]

def ask(messages):
    headers = {"Content-Type": "application/json", "anthropic-version": "2023-06-01"}
    headers["Authorization" if OPENROUTER_KEY else "x-api-key"] = (f"Bearer {KEY}" if OPENROUTER_KEY else KEY)
    req = urllib.request.Request(
        API,
        json.dumps({"model": MODEL, "max_tokens": 8192, "system": "Concise assistant", "messages": messages, "tools": schema}).encode(),
        headers,
    )
    return json.loads(urllib.request.urlopen(req).read())

def main():
    device = "GPU (Triton)" if torch.cuda.is_available() else "CPU (Triton unavailable)"
    print(f"{B}nanocode{R} | {D}{MODEL} | {device}{R}\n")
    messages = []
    
    while True:
        try:
            user_input = input(f"{B}{BL}❯{R} ").strip()
            if not user_input: continue
            if user_input in ("/q", "exit"): break
            if user_input == "/c": messages = []; print(f"{G}⏺ Cleared{R}"); continue
            
            # GPU-accelerate input processing if available
            if torch.cuda.is_available():
                user_input = gpu_process(user_input)
            
            messages.append({"role": "user", "content": user_input})
            while True:
                resp = ask(messages)
                content = resp.get("content", [])
                results = []
                for block in content:
                    if block["type"] == "text": print(f"\n{C}⏺{R} {block['text']}")
                    if block["type"] == "tool_use":
                        print(f"\n{G}⏺ {block['name']}{R}")
                        result = tools[block["name"]](block["input"])
                        print(f"  {D}⎿ {result.split(chr(10))[0][:60]}{R}")
                        results.append({"type": "tool_result", "tool_use_id": block["id"], "content": result})
                messages.append({"role": "assistant", "content": content})
                if not results: break
                messages.append({"role": "user", "content": results})
            print()
        except (KeyboardInterrupt, EOFError): break

if __name__ == "__main__": main()
