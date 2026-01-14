// nanocode - minimal claude code alternative (Hono web UI)
// bun add hono && bun run nanocode.ts
import { Hono } from "hono";
import { html } from "hono/html";
import { readFileSync, writeFileSync } from "fs";
import { execSync } from "child_process";

const KEY = process.env.ANTHROPIC_API_KEY!;
const MODEL = process.env.MODEL || "claude-sonnet-4-20250514";
const messages: any[] = [];

const tools: Record<string, (a: any) => string> = {
  read: (a) => readFileSync(a.path, "utf8").split("\n").map((l, i) => `${i + 1}| ${l}`).join("\n"),
  write: (a) => (writeFileSync(a.path, a.content), "ok"),
  bash: (a) => { try { return execSync(a.cmd, { encoding: "utf8", timeout: 30000 }); } catch (e: any) { return e.message; } },
};

const schema = [
  { name: "read", description: "Read file", input_schema: { type: "object", properties: { path: { type: "string" } }, required: ["path"] } },
  { name: "write", description: "Write file", input_schema: { type: "object", properties: { path: { type: "string" }, content: { type: "string" } }, required: ["path", "content"] } },
  { name: "bash", description: "Run command", input_schema: { type: "object", properties: { cmd: { type: "string" } }, required: ["cmd"] } },
];

async function ask() {
  const res = await fetch("https://api.anthropic.com/v1/messages", {
    method: "POST",
    headers: { "Content-Type": "application/json", "anthropic-version": "2023-06-01", "x-api-key": KEY },
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
