// nanocode - minimal claude code alternative (Elysia web UI)
// bun add elysia && bun run nanocode.ts
import { Elysia, t } from "elysia";
import { readFileSync, writeFileSync } from "fs";
import { execSync } from "child_process";

const KEY = process.env.ANTHROPIC_API_KEY!;
const MODEL = process.env.MODEL || "claude-sonnet-4-20250514";
const messages: any[] = [];

const tools: Record<string, (a: any) => string> = {
  read: (a) => readFileSync(a.path, "utf8").split("\n").map((l, i) => `${i + 1}| ${l}`).join("\n"),
  write: (a) => (writeFileSync(a.path, a.content), "ok"),
  bash: (a) => { try { return execSync(a.cmd, { encoding: "utf8" }); } catch (e: any) { return e.message; } },
};

const schema = [
  { name: "read", description: "Read", input_schema: { type: "object", properties: { path: { type: "string" } }, required: ["path"] } },
  { name: "write", description: "Write", input_schema: { type: "object", properties: { path: { type: "string" }, content: { type: "string" } }, required: ["path", "content"] } },
  { name: "bash", description: "Run", input_schema: { type: "object", properties: { cmd: { type: "string" } }, required: ["cmd"] } },
];

async function ask() {
  return fetch("https://api.anthropic.com/v1/messages", {
    method: "POST",
    headers: { "Content-Type": "application/json", "anthropic-version": "2023-06-01", "x-api-key": KEY },
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
