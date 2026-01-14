#!/usr/bin/env -S node --experimental-strip-types
// nanocode - minimal claude code alternative (Node.js)
import { createInterface } from "readline";
import { execSync } from "child_process";
import { readFileSync, writeFileSync, readdirSync, statSync } from "fs";
import { join } from "path";

const KEY = process.env.OPENROUTER_API_KEY || process.env.ANTHROPIC_API_KEY;
const API = process.env.OPENROUTER_API_KEY ? "https://openrouter.ai/api/v1/messages" : "https://api.anthropic.com/v1/messages";
const MODEL = process.env.MODEL || (process.env.OPENROUTER_API_KEY ? "anthropic/claude-opus-4" : "claude-sonnet-4-20250514");
const [R, B, D, C, G] = ["\x1b[0m", "\x1b[1m", "\x1b[2m", "\x1b[36m", "\x1b[32m"];

const tools: Record<string, [(a: any) => string, string, object]> = {
  read: [(a) => readFileSync(a.path, "utf8").split("\n").slice(a.offset||0, (a.offset||0)+(a.limit||Infinity)).map((l,i) => `${a.offset||0+i+1}| ${l}`).join("\n"), "Read file", {path:"string",offset:"number?",limit:"number?"}],
  write: [(a) => (writeFileSync(a.path, a.content), "ok"), "Write file", {path:"string",content:"string"}],
  edit: [(a) => { let t = readFileSync(a.path,"utf8"); if(!t.includes(a.old)) return "error: not found"; if(!a.all && t.split(a.old).length>2) return "error: not unique"; writeFileSync(a.path, a.all?t.replaceAll(a.old,a.new):t.replace(a.old,a.new)); return "ok"; }, "Edit file", {path:"string",old:"string",new:"string",all:"boolean?"}],
  glob: [(a) => { const find = (d: string, p: string): string[] => { try { return readdirSync(d,{withFileTypes:true}).flatMap(e => e.isDirectory() ? find(join(d,e.name),p) : join(d,e.name).includes(p.replace("**/*",""))?[join(d,e.name)]:[]); } catch { return []; }}; return find(a.path||".", a.pat).slice(0,50).join("\n")||"none"; }, "Find files", {pat:"string",path:"string?"}],
  grep: [(a) => { const find = (d: string): string[] => { try { return readdirSync(d,{withFileTypes:true}).flatMap(e => e.isDirectory() ? find(join(d,e.name)) : [join(d,e.name)]); } catch { return []; }}; const re = new RegExp(a.pat); return find(a.path||".").flatMap(f => { try { return readFileSync(f,"utf8").split("\n").map((l,i)=>[f,i+1,l] as const).filter(([_,__,l])=>re.test(l)).map(([f,n,l])=>`${f}:${n}:${l}`); } catch { return []; }}).slice(0,50).join("\n")||"none"; }, "Search files", {pat:"string",path:"string?"}],
  bash: [(a) => { try { return execSync(a.cmd,{encoding:"utf8",timeout:30000}); } catch(e:any) { return e.stdout||e.message; }}, "Run command", {cmd:"string"}],
};

const schema = Object.entries(tools).map(([name, [_, desc, params]]) => ({
  name, description: desc,
  input_schema: { type: "object", properties: Object.fromEntries(Object.entries(params).map(([k,v]) => [k, {type: (v as string).replace("?","")==="number"?"integer":((v as string).replace("?",""))}])), required: Object.entries(params).filter(([_,v]) => !(v as string).endsWith("?")).map(([k]) => k) }
}));

async function ask(messages: any[]) {
  const res = await fetch(API, {
    method: "POST",
    headers: { "Content-Type": "application/json", "anthropic-version": "2023-06-01", ...(process.env.OPENROUTER_API_KEY ? { Authorization: `Bearer ${KEY}` } : { "x-api-key": KEY! }) },
    body: JSON.stringify({ model: MODEL, max_tokens: 8192, system: `Concise coding assistant. cwd: ${process.cwd()}`, messages, tools: schema })
  });
  return res.json();
}

async function main() {
  console.log(`${B}nanocode${R} | ${D}${MODEL} | ${process.cwd()}${R}\n`);
  const rl = createInterface({ input: process.stdin, output: process.stdout });
  const messages: any[] = [];

  const prompt = () => rl.question(`${B}\x1b[34m❯${R} `, async (input) => {
    if (!input.trim()) return prompt();
    if (input === "/q") return rl.close();
    if (input === "/c") { messages.length = 0; console.log(`${G}⏺ Cleared${R}`); return prompt(); }
    
    messages.push({ role: "user", content: input });
    while (true) {
      const { content } = await ask(messages);
      const results: any[] = [];
      for (const block of content) {
        if (block.type === "text") console.log(`\n${C}⏺${R} ${block.text}`);
        if (block.type === "tool_use") {
          console.log(`\n${G}⏺ ${block.name}${R}(${D}${JSON.stringify(block.input).slice(0,50)}${R})`);
          const result = tools[block.name][0](block.input);
          console.log(`  ${D}⎿ ${result.split("\n")[0].slice(0,60)}${R}`);
          results.push({ type: "tool_result", tool_use_id: block.id, content: result });
        }
      }
      messages.push({ role: "assistant", content });
      if (!results.length) break;
      messages.push({ role: "user", content: results });
    }
    console.log(); prompt();
  });
  prompt();
}

main();
