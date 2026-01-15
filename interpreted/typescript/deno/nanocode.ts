#!/usr/bin/env -S deno run -A
// nanocode - minimal claude code alternative (Deno)
const KEY = Deno.env.get("OPENROUTER_API_KEY") || Deno.env.get("ANTHROPIC_API_KEY");
const API = Deno.env.get("API_URL") || (Deno.env.get("OPENROUTER_API_KEY") ? "https://openrouter.ai/api/v1/messages" : "https://api.anthropic.com/v1/messages");
const MODEL = Deno.env.get("MODEL") || (Deno.env.get("OPENROUTER_API_KEY") ? "anthropic/claude-opus-4" : "claude-sonnet-4-20250514");
const [R, B, D, C, G] = ["\x1b[0m", "\x1b[1m", "\x1b[2m", "\x1b[36m", "\x1b[32m"];

const tools: Record<string, [(a: any) => string|Promise<string>, string, object]> = {
  read: [(a) => Deno.readTextFileSync(a.path).split("\n").slice(a.offset||0,(a.offset||0)+(a.limit||Infinity)).map((l,i)=>`${(a.offset||0)+i+1}| ${l}`).join("\n"), "Read file", {path:"string",offset:"number?",limit:"number?"}],
  write: [(a) => (Deno.writeTextFileSync(a.path, a.content), "ok"), "Write file", {path:"string",content:"string"}],
  edit: [(a) => { const t=Deno.readTextFileSync(a.path); if(!t.includes(a.old)) return "error: not found"; if(!a.all&&t.split(a.old).length>2) return "error: not unique"; Deno.writeTextFileSync(a.path, a.all?t.replaceAll(a.old,a.new):t.replace(a.old,a.new)); return "ok"; }, "Edit file", {path:"string",old:"string",new:"string",all:"boolean?"}],
  glob: [(a) => [...Deno.readDirSync(a.path||".")].flatMap(e => e.isDirectory ? [] : [e.name]).slice(0,50).join("\n")||"none", "Find files", {pat:"string",path:"string?"}],
  grep: [(a) => { const re = new RegExp(a.pat); const walk = (d:string):string[] => { try { return [...Deno.readDirSync(d)].flatMap(e => e.isDirectory ? walk(`${d}/${e.name}`) : [`${d}/${e.name}`]); } catch { return []; }}; return walk(a.path||".").flatMap(f => { try { return Deno.readTextFileSync(f).split("\n").map((l,i)=>[f,i+1,l] as const).filter(([,,l])=>re.test(l)).map(([f,n,l])=>`${f}:${n}:${l}`); } catch { return []; }}).slice(0,50).join("\n")||"none"; }, "Search", {pat:"string",path:"string?"}],
  bash: [async (a) => new TextDecoder().decode((await new Deno.Command("sh", {args:["-c",a.cmd]}).output()).stdout), "Run command", {cmd:"string"}],
};

const schema = Object.entries(tools).map(([name, [_, desc, params]]) => ({
  name, description: desc,
  input_schema: { type: "object", properties: Object.fromEntries(Object.entries(params).map(([k,v]) => [k, {type:(v as string).replace("?","")==="number"?"integer":(v as string).replace("?","")}])), required: Object.entries(params).filter(([_,v]) => !(v as string).endsWith("?")).map(([k]) => k) }
}));

async function ask(messages: any[]) {
  const res = await fetch(API, {
    method: "POST", headers: { "Content-Type": "application/json", "anthropic-version": "2023-06-01", ...(Deno.env.get("OPENROUTER_API_KEY") ? { Authorization: `Bearer ${KEY}` } : { "x-api-key": KEY! }) },
    body: JSON.stringify({ model: MODEL, max_tokens: 8192, system: `Concise coding assistant. cwd: ${Deno.cwd()}`, messages, tools: schema })
  });
  return res.json();
}

console.log(`${B}nanocode${R} | ${D}${MODEL}${R}\n`);
const messages: any[] = [];
const buf = new Uint8Array(1024);

while (true) {
  await Deno.stdout.write(new TextEncoder().encode(`${B}\x1b[34m❯${R} `));
  const n = await Deno.stdin.read(buf);
  if (n === null) break;
  const input = new TextDecoder().decode(buf.subarray(0, n)).trim();
  if (!input) continue;
  if (input === "/q") break;
  if (input === "/c") { messages.length = 0; console.log(`${G}⏺ Cleared${R}`); continue; }
  
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
  console.log();
}
