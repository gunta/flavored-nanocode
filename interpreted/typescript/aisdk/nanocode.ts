#!/usr/bin/env bun
// nanocode - minimal claude code alternative (Vercel AI SDK)
import { anthropic } from "@ai-sdk/anthropic";
import { generateText, tool } from "ai";
import { z } from "zod";
import { readFileSync, writeFileSync } from "fs";
import { execSync } from "child_process";
import { createInterface } from "readline";

const [R, B, D, C, G] = ["\x1b[0m", "\x1b[1m", "\x1b[2m", "\x1b[36m", "\x1b[32m"];

const tools = {
  read: tool({ description: "Read file", parameters: z.object({ path: z.string(), offset: z.number().optional(), limit: z.number().optional() }),
    execute: async ({ path, offset = 0, limit = Infinity }) => readFileSync(path, "utf8").split("\n").slice(offset, offset + limit).map((l, i) => `${offset + i + 1}| ${l}`).join("\n") }),
  write: tool({ description: "Write file", parameters: z.object({ path: z.string(), content: z.string() }),
    execute: async ({ path, content }) => (writeFileSync(path, content), "ok") }),
  edit: tool({ description: "Edit file", parameters: z.object({ path: z.string(), old: z.string(), new: z.string(), all: z.boolean().optional() }),
    execute: async ({ path, old, new: n, all }) => { const t = readFileSync(path, "utf8"); if (!t.includes(old)) return "error: not found"; if (!all && t.split(old).length > 2) return "error: not unique"; writeFileSync(path, all ? t.replaceAll(old, n) : t.replace(old, n)); return "ok"; } }),
  glob: tool({ description: "Find files", parameters: z.object({ pat: z.string(), path: z.string().optional() }),
    execute: async ({ pat, path = "." }) => execSync(`find ${path} -name "${pat}" 2>/dev/null | head -50`, { encoding: "utf8" }) || "none" }),
  grep: tool({ description: "Search files", parameters: z.object({ pat: z.string(), path: z.string().optional() }),
    execute: async ({ pat, path = "." }) => { try { return execSync(`grep -rn "${pat}" ${path} 2>/dev/null | head -50`, { encoding: "utf8" }); } catch { return "none"; } } }),
  bash: tool({ description: "Run command", parameters: z.object({ cmd: z.string() }),
    execute: async ({ cmd }) => { try { return execSync(cmd, { encoding: "utf8", timeout: 30000 }); } catch (e: any) { return e.stdout || e.message; } } }),
};

console.log(`${B}nanocode${R} | ${D}AI SDK + Claude${R}\n`);
const rl = createInterface({ input: process.stdin, output: process.stdout });
const messages: any[] = [];

const prompt = () => rl.question(`${B}\x1b[34m❯${R} `, async (input) => {
  if (!input.trim()) return prompt();
  if (input === "/q") return rl.close();
  if (input === "/c") { messages.length = 0; console.log(`${G}⏺ Cleared${R}`); return prompt(); }

  messages.push({ role: "user", content: input });
  const { text, toolCalls, toolResults } = await generateText({
    model: anthropic("claude-sonnet-4-20250514"), maxTokens: 8192, tools, maxSteps: 10,
    system: `Concise coding assistant. cwd: ${process.cwd()}`, messages,
    onStepFinish: ({ text, toolCalls }) => {
      if (text) console.log(`\n${C}⏺${R} ${text}`);
      toolCalls?.forEach(t => console.log(`\n${G}⏺ ${t.toolName}${R}(${D}${JSON.stringify(t.args).slice(0,50)}${R})`));
    }
  });
  messages.push({ role: "assistant", content: text });
  console.log(); prompt();
});
prompt();
