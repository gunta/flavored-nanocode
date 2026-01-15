// nanocode - minimal claude code alternative (Elysia web UI)
// bun add elysia && bun run nanocode.ts
import { Elysia, t } from "elysia";
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
    const txt = readFileSync(a.path, "utf8");
    const count = txt.split(a.old).length - 1;
    if (count === 0) return "error: old_string not found";
    if (count > 1 && !a.all) return `error: old_string appears ${count} times, use all=true`;
    const updated = a.all ? txt.split(a.old).join(a.new) : txt.replace(a.old, a.new);
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
  bash: (a) => { try { return execSync(a.cmd, { encoding: "utf8" }); } catch (e: any) { return e.message; } },
};

const schema = [
  { name: "read", description: "Read", input_schema: { type: "object", properties: { path: { type: "string" }, offset: { type: "integer" }, limit: { type: "integer" } }, required: ["path"] } },
  { name: "write", description: "Write", input_schema: { type: "object", properties: { path: { type: "string" }, content: { type: "string" } }, required: ["path", "content"] } },
  { name: "edit", description: "Replace", input_schema: { type: "object", properties: { path: { type: "string" }, old: { type: "string" }, new: { type: "string" }, all: { type: "boolean" } }, required: ["path", "old", "new"] } },
  { name: "glob", description: "Find files", input_schema: { type: "object", properties: { pat: { type: "string" }, path: { type: "string" } }, required: ["pat"] } },
  { name: "grep", description: "Search files", input_schema: { type: "object", properties: { pat: { type: "string" }, path: { type: "string" } }, required: ["pat"] } },
  { name: "bash", description: "Run", input_schema: { type: "object", properties: { cmd: { type: "string" } }, required: ["cmd"] } },
];

async function ask() {
  return fetch(API, {
    method: "POST",
    headers: { "Content-Type": "application/json", "anthropic-version": "2023-06-01", ...(process.env.OPENROUTER_API_KEY ? { Authorization: `Bearer ${KEY}` } : { "x-api-key": KEY }) },
    body: JSON.stringify({ model: MODEL, max_tokens: 4096, system: "Concise assistant", messages, tools: schema }),
  }).then((r) => r.json());
}

const HTML = `<!DOCTYPE html><html><head><title>nanocode</title>
<style>body{font:14px monospace;background:#0d1117;color:#c9d1d9;max-width:800px;margin:0 auto;padding:20px}
#out{white-space:pre-wrap}form{display:flex}input{flex:1;background:#21262d;border:1px solid #30363d;color:#c9d1d9;padding:8px}
button{background:#238636;border:none;color:#fff;padding:8px 16px}</style></head>
<body><h2>🧬 nanocode (Elysia)</h2><div id="out"></div>
<form onsubmit="send(event)"><input id="i" autofocus><button>→</button></form>
<script>async function send(e){e.preventDefault();const m=i.value;i.value='';out.innerHTML+='❯ '+m+'\\n';
const r=await(await fetch('/chat',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({msg:m})})).json();
out.innerHTML+=r.map(b=>b.type==='text'?'⏺ '+b.text:'⚙ '+b.name).join('\\n')+'\\n\\n';}</script></body></html>`;

const app = new Elysia()
  .get("/", () => new Response(HTML, { headers: { "Content-Type": "text/html" } }))
  .post("/chat", async ({ body }: { body: { msg: string } }) => {
    if (body.msg === "/c") { messages.length = 0; return [{ type: "text", text: "Cleared" }]; }
    messages.push({ role: "user", content: body.msg });
    const out: any[] = [];
    while (true) {
      const { content } = await ask();
      for (const b of content) {
        if (b.type === "text") out.push({ type: "text", text: b.text });
        if (b.type === "tool_use") {
          out.push({ type: "tool", name: b.name });
          const r = tools[b.name](b.input);
          messages.push({ role: "assistant", content }, { role: "user", content: [{ type: "tool_result", tool_use_id: b.id, content: r }] });
        }
      }
      if (!content.some((b: any) => b.type === "tool_use")) { messages.push({ role: "assistant", content }); break; }
    }
    return out;
  }, { body: t.Object({ msg: t.String() }) })
  .listen(3000);

console.log(`nanocode running on http://localhost:${app.server?.port}`);
