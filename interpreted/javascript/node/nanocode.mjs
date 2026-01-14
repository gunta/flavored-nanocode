#!/usr/bin/env node
// nanocode - minimal claude code alternative (Node.js ESM)
import { createInterface } from "readline";
import { execSync } from "child_process";
import { readFileSync, writeFileSync, readdirSync } from "fs";
import { join } from "path";

const KEY = process.env.OPENROUTER_API_KEY || process.env.ANTHROPIC_API_KEY;
const API = process.env.OPENROUTER_API_KEY ? "https://openrouter.ai/api/v1/messages" : "https://api.anthropic.com/v1/messages";
const MODEL = process.env.MODEL || (process.env.OPENROUTER_API_KEY ? "anthropic/claude-opus-4" : "claude-sonnet-4-20250514");
const [R, B, D, C, G] = ["\x1b[0m", "\x1b[1m", "\x1b[2m", "\x1b[36m", "\x1b[32m"];

const walk = (d) => { try { return readdirSync(d,{withFileTypes:1}).flatMap(e => e.isDirectory() ? walk(join(d,e.name)) : [join(d,e.name)]); } catch { return []; }};
const tools = {
  read: (a) => readFileSync(a.path,"utf8").split("\n").slice(a.offset||0,(a.offset||0)+(a.limit||1e9)).map((l,i)=>`${(a.offset||0)+i+1}| ${l}`).join("\n"),
  write: (a) => (writeFileSync(a.path, a.content), "ok"),
  edit: (a) => { const t=readFileSync(a.path,"utf8"); if(!t.includes(a.old)) return "error: not found"; if(!a.all&&t.split(a.old).length>2) return "error: not unique"; writeFileSync(a.path,a.all?t.replaceAll(a.old,a.new):t.replace(a.old,a.new)); return "ok"; },
  glob: (a) => walk(a.path||".").filter(f=>f.includes(a.pat.replace(/\*+/g,""))).slice(0,50).join("\n")||"none",
  grep: (a) => { const re=new RegExp(a.pat); return walk(a.path||".").flatMap(f=>{ try { return readFileSync(f,"utf8").split("\n").map((l,i)=>[f,i+1,l]).filter(([,,l])=>re.test(l)).map(([f,n,l])=>`${f}:${n}:${l}`); } catch { return []; }}).slice(0,50).join("\n")||"none"; },
  bash: (a) => { try { return execSync(a.cmd,{encoding:"utf8",timeout:30000}); } catch(e) { return e.stdout||e.message; }},
};
const schema = [
  {name:"read",description:"Read file",input_schema:{type:"object",properties:{path:{type:"string"},offset:{type:"integer"},limit:{type:"integer"}},required:["path"]}},
  {name:"write",description:"Write file",input_schema:{type:"object",properties:{path:{type:"string"},content:{type:"string"}},required:["path","content"]}},
  {name:"edit",description:"Edit file",input_schema:{type:"object",properties:{path:{type:"string"},old:{type:"string"},new:{type:"string"},all:{type:"boolean"}},required:["path","old","new"]}},
  {name:"glob",description:"Find files",input_schema:{type:"object",properties:{pat:{type:"string"},path:{type:"string"}},required:["pat"]}},
  {name:"grep",description:"Search files",input_schema:{type:"object",properties:{pat:{type:"string"},path:{type:"string"}},required:["pat"]}},
  {name:"bash",description:"Run command",input_schema:{type:"object",properties:{cmd:{type:"string"}},required:["cmd"]}},
];

async function ask(messages) {
  const res = await fetch(API, {
    method: "POST",
    headers: { "Content-Type": "application/json", "anthropic-version": "2023-06-01", ...(process.env.OPENROUTER_API_KEY ? { Authorization: `Bearer ${KEY}` } : { "x-api-key": KEY }) },
    body: JSON.stringify({ model: MODEL, max_tokens: 8192, system: `Concise coding assistant. cwd: ${process.cwd()}`, messages, tools: schema })
  });
  return res.json();
}

console.log(`${B}nanocode${R} | ${D}${MODEL}${R}\n`);
const rl = createInterface({ input: process.stdin, output: process.stdout });
const messages = [];

const prompt = () => rl.question(`${B}\x1b[34m❯${R} `, async (input) => {
  if (!input.trim()) return prompt();
  if (input === "/q") return rl.close();
  if (input === "/c") { messages.length = 0; console.log(`${G}⏺ Cleared${R}`); return prompt(); }
  
  messages.push({ role: "user", content: input });
  while (1) {
    const { content } = await ask(messages);
    const results = [];
    for (const b of content) {
      if (b.type === "text") console.log(`\n${C}⏺${R} ${b.text}`);
      if (b.type === "tool_use") {
        console.log(`\n${G}⏺ ${b.name}${R}(${D}${JSON.stringify(b.input).slice(0,50)}${R})`);
        const r = tools[b.name](b.input);
        console.log(`  ${D}⎿ ${r.split("\n")[0].slice(0,60)}${R}`);
        results.push({ type: "tool_result", tool_use_id: b.id, content: r });
      }
    }
    messages.push({ role: "assistant", content });
    if (!results.length) break;
    messages.push({ role: "user", content: results });
  }
  console.log(); prompt();
});
prompt();
