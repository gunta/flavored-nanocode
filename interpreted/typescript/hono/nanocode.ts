// nanocode - minimal claude code alternative (Hono web UI)
// bun add hono && bun run nanocode.ts
import { Hono } from "hono";
import { html } from "hono/html";
import { readFileSync, writeFileSync } from "fs";
import { execSync } from "child_process";

const KEY = process.env.OPENROUTER_API_KEY || process.env.ANTHROPIC_API_KEY!;
const API = process.env.API_URL || (process.env.OPENROUTER_API_KEY ? "https://openrouter.ai/api/v1/messages" : "https://api.anthropic.com/v1/messages");
const MODEL = process.env.MODEL || (process.env.OPENROUTER_API_KEY ? "anthropic/claude-opus-4-5" : "claude-opus-4-5");
const messages: any[] = [];

const tools: Record<string, (a: any) => string> = {
  read: (a) => {
    const lines = readFileSync(a.path, "utf8").split("\n");
    const off = a.offset ?? 0;
    const lim = a.limit ?? lines.length;
    return lines.slice(off, off + lim).map((l: string, i: number) => `${off + i + 1}| ${l}`).join("\n");
  },
  write: (a) => (writeFileSync(a.path, a.content), "ok"),
  edit: (a) => {
    const text = readFileSync(a.path, "utf8");
    const count = text.split(a.old).length - 1;
    if (count === 0) return "error: old_string not found";
    if (count > 1 && !a.all) return `error: old_string appears ${count} times, use all=true`;
    const updated = a.all ? text.split(a.old).join(a.new) : text.replace(a.old, a.new);
    writeFileSync(a.path, updated);
    return "ok";
  },
  glob: (a) => {
    const base = a.path || ".";
    try { return execSync(`find ${base} -name '${a.pat}' | head -50`, { encoding: "utf8" }); } catch (e: any) { return e.message; }
  },
  grep: (a) => {
    const base = a.path || ".";
    try { const out = execSync(`grep -R "${a.pat}" ${base} -n 2>/dev/null | head -50`, { encoding: "utf8" }); return out || "none"; } catch { return "none"; }
  },
  bash: (a) => { try { return execSync(a.cmd, { encoding: "utf8", timeout: 30000 }); } catch (e: any) { return e.message; } },
};

const schema = [
  { name: "read", description: "Read file", input_schema: { type: "object", properties: { path: { type: "string" }, offset: { type: "integer" }, limit: { type: "integer" } }, required: ["path"] } },
  { name: "write", description: "Write file", input_schema: { type: "object", properties: { path: { type: "string" }, content: { type: "string" } }, required: ["path", "content"] } },
  { name: "edit", description: "Replace text", input_schema: { type: "object", properties: { path: { type: "string" }, old: { type: "string" }, new: { type: "string" }, all: { type: "boolean" } }, required: ["path", "old", "new"] } },
  { name: "glob", description: "Find files", input_schema: { type: "object", properties: { pat: { type: "string" }, path: { type: "string" } }, required: ["pat"] } },
  { name: "grep", description: "Search files", input_schema: { type: "object", properties: { pat: { type: "string" }, path: { type: "string" } }, required: ["pat"] } },
  { name: "bash", description: "Run command", input_schema: { type: "object", properties: { cmd: { type: "string" } }, required: ["cmd"] } },
];

async function ask() {
  const res = await fetch(API, {
    method: "POST",
    headers: { "Content-Type": "application/json", "anthropic-version": "2023-06-01", ...(process.env.OPENROUTER_API_KEY ? { Authorization: `Bearer ${KEY}` } : { "x-api-key": KEY }) },
    body: JSON.stringify({ model: MODEL, max_tokens: 4096, system: "Concise coding assistant", messages, tools: schema }),
  });
  return res.json();
}

const app = new Hono();

app.get("/", (c) => c.html(html`<!DOCTYPE html>
<html><head><title>nanocode</title>
<style>body{font:14px monospace;background:#0d1117;color:#c9d1d9;max-width:800px;margin:0 auto;padding:20px}
#out{white-space:pre-wrap;margin-bottom:20px}form{display:flex}input{flex:1;background:#21262d;border:1px solid #30363d;color:#c9d1d9;padding:8px;font:inherit}
button{background:#238636;border:none;color:#fff;padding:8px 16px;cursor:pointer}.tool{color:#3fb950}.text{color:#58a6ff}</style></head>
<body><h2>🧬 nanocode</h2><div id="out"></div>
<form onsubmit="send(event)"><input id="i" placeholder="Enter your message..." autofocus><button>Send</button></form>
<script>const out=document.getElementById('out'),inp=document.getElementById('i');
async function send(e){e.preventDefault();const m=inp.value;inp.value='';out.innerHTML+='<b>❯</b> '+m+'\\n';
const r=await fetch('/chat',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({message:m})});
const d=await r.json();d.forEach(b=>out.innerHTML+=b.type==='text'?'<span class="text">⏺</span> '+b.text+'\\n':
'<span class="tool">⏺</span> '+b.name+'\\n');out.innerHTML+='\\n';}</script></body></html>`));

app.post("/chat", async (c) => {
  const { message } = await c.req.json();
  if (message === "/c") { messages.length = 0; return c.json([{ type: "text", text: "Cleared" }]); }
  
  messages.push({ role: "user", content: message });
  const output: any[] = [];
  
  while (true) {
    const { content } = await ask();
    for (const block of content) {
      if (block.type === "text") output.push({ type: "text", text: block.text });
      if (block.type === "tool_use") {
        const result = tools[block.name](block.input);
        output.push({ type: "tool", name: block.name, result: result.split("\n")[0] });
        messages.push({ role: "assistant", content }, { role: "user", content: [{ type: "tool_result", tool_use_id: block.id, content: result }] });
      }
    }
    if (!content.some((b: any) => b.type === "tool_use")) { messages.push({ role: "assistant", content }); break; }
  }
  return c.json(output);
});

export default { port: 3000, fetch: app.fetch };
console.log("nanocode running on http://localhost:3000");
