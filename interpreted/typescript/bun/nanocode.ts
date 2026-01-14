#!/usr/bin/env bun
// nanocode - minimal claude code alternative (Bun)
import { $ } from "bun";

const KEY = Bun.env.OPENROUTER_API_KEY || Bun.env.ANTHROPIC_API_KEY;
const API = Bun.env.OPENROUTER_API_KEY ? "https://openrouter.ai/api/v1/messages" : "https://api.anthropic.com/v1/messages";
const MODEL = Bun.env.MODEL || (Bun.env.OPENROUTER_API_KEY ? "anthropic/claude-opus-4" : "claude-sonnet-4-20250514");
const [R, B, D, C, G] = ["\x1b[0m", "\x1b[1m", "\x1b[2m", "\x1b[36m", "\x1b[32m"];

const tools: Record<string, [(a: any) => string|Promise<string>, string, object]> = {
  read: [(a) => Bun.file(a.path).text().then(t => t.split("\n").slice(a.offset||0, (a.offset||0)+(a.limit||Infinity)).map((l,i) => `${(a.offset||0)+i+1}| ${l}`).join("\n")), "Read file", {path:"string",offset:"number?",limit:"number?"}],
  write: [(a) => (Bun.write(a.path, a.content), "ok"), "Write file", {path:"string",content:"string"}],
  edit: [async (a) => { const t = await Bun.file(a.path).text(); if(!t.includes(a.old)) return "error: not found"; if(!a.all && t.split(a.old).length>2) return "error: not unique"; await Bun.write(a.path, a.all?t.replaceAll(a.old,a.new):t.replace(a.old,a.new)); return "ok"; }, "Edit file", {path:"string",old:"string",new:"string",all:"boolean?"}],
  glob: [async (a) => { const g = new Bun.Glob(a.pat); return [...g.scanSync(a.path||".")].slice(0,50).join("\n")||"none"; }, "Find files", {pat:"string",path:"string?"}],
  grep: [async (a) => { const g = new Bun.Glob("**/*"); const re = new RegExp(a.pat); return [...g.scanSync(a.path||".")].flatMap(f => { try { const lines = Bun.file(f).text(); return []; } catch { return []; }}).join("\n")||"none"; }, "Search", {pat:"string",path:"string?"}],
  bash: [async (a) => { try { return await $`${a.cmd}`.text(); } catch(e:any) { return e.message; }}, "Run command", {cmd:"string"}],
};

const schema = Object.entries(tools).map(([name, [_, desc, params]]) => ({
  name, description: desc,
  input_schema: { type: "object", properties: Object.fromEntries(Object.entries(params).map(([k,v]) => [k, {type:(v as string).replace("?","")==="number"?"integer":(v as string).replace("?","")}])), required: Object.entries(params).filter(([_,v]) => !(v as string).endsWith("?")).map(([k]) => k) }
}));

async function ask(messages: any[]) {
  const res = await fetch(API, {
    method: "POST", headers: { "Content-Type": "application/json", "anthropic-version": "2023-06-01", ...(Bun.env.OPENROUTER_API_KEY ? { Authorization: `Bearer ${KEY}` } : { "x-api-key": KEY! }) },
    body: JSON.stringify({ model: MODEL, max_tokens: 8192, system: `Concise coding assistant. cwd: ${process.cwd()}`, messages, tools: schema })
  });
  return res.json();
}

console.log(`${B}nanocode${R} | ${D}${MODEL}${R}\n`);
const messages: any[] = [];

for await (const line of console.write(`${B}\x1b[34m❯${R} `), Bun.stdin.stream()) {
  const input = new TextDecoder().decode(line).trim();
  if (!input) { console.write(`${B}\x1b[34m❯${R} `); continue; }
  if (input === "/q") break;
  if (input === "/c") { messages.length = 0; console.log(`${G}⏺ Cleared${R}`); console.write(`${B}\x1b[34m❯${R} `); continue; }
  
  messages.push({ role: "user", content: input });
  while (true) {
    const { content } = await ask(messages);
    const results: any[] = [];
    for (const block of content) {
      if (block.type === "text") console.log(`\n${C}⏺${R} ${block.text}`);
      if (block.type === "tool_use") {
        console.log(`\n${G}⏺ ${block.name}${R}(${D}${JSON.stringify(block.input).slice(0,50)}${R})`);
        const result = await tools[block.name][0](block.input);
        console.log(`  ${D}⎿ ${result.split("\n")[0].slice(0,60)}${R}`);
        results.push({ type: "tool_result", tool_use_id: block.id, content: result });
      }
    }
    messages.push({ role: "assistant", content });
    if (!results.length) break;
    messages.push({ role: "user", content: results });
  }
  console.log(); console.write(`${B}\x1b[34m❯${R} `);
}
