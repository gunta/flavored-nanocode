#!/usr/bin/env bun
// nanocode - minimal claude code alternative (Effect-TS + Effect AI)
// bun add effect @effect/ai @effect/ai-anthropic @effect/platform @effect/platform-node && bun run nanocode.ts
import { Effect, pipe, Console, Ref, Layer, Config, Schema } from "effect";
import { LanguageModel, Tool, Toolkit } from "@effect/ai";
import { AnthropicClient, AnthropicLanguageModel } from "@effect/ai-anthropic";
import { FileSystem, Command, Terminal } from "@effect/platform";
import { NodeFileSystem, NodeRuntime, NodeHttpClient, NodeCommandExecutor, NodeTerminal } from "@effect/platform-node";

const [R, B, D, C, G, BL] = ["\x1b[0m", "\x1b[1m", "\x1b[2m", "\x1b[36m", "\x1b[32m", "\x1b[34m"];

// --- Config ---

const AppConfig = Config.all({
  model: Config.string("MODEL").pipe(Config.withDefault("claude-sonnet-4-20250514")),
});

// --- Tool definitions with Effect Schema ---

const ReadTool = Tool.make("read", {
  description: "Read file contents with line numbers",
  parameters: { path: Schema.String.annotations({ description: "File path to read" }) },
  success: Schema.String,
});

const WriteTool = Tool.make("write", {
  description: "Write content to a file",
  parameters: {
    path: Schema.String.annotations({ description: "File path to write" }),
    content: Schema.String.annotations({ description: "Content to write" }),
  },
  success: Schema.String,
});

const EditTool = Tool.make("edit", {
  description: "Edit file by replacing text",
  parameters: {
    path: Schema.String.annotations({ description: "File path to edit" }),
    old: Schema.String.annotations({ description: "Text to find" }),
    new: Schema.String.annotations({ description: "Replacement text" }),
  },
  success: Schema.String,
});

const BashTool = Tool.make("bash", {
  description: "Run a shell command",
  parameters: { cmd: Schema.String.annotations({ description: "Command to run" }) },
  success: Schema.String,
});

const GlobTool = Tool.make("glob", {
  description: "Find files matching a pattern",
  parameters: { pat: Schema.String.annotations({ description: "Glob pattern" }) },
  success: Schema.String,
});

const GrepTool = Tool.make("grep", {
  description: "Search for pattern in files",
  parameters: { pat: Schema.String.annotations({ description: "Search pattern" }) },
  success: Schema.String,
});

// --- Toolkit ---

const NanocodeTools = Toolkit.make(ReadTool, WriteTool, EditTool, BashTool, GlobTool, GrepTool);

// --- Tool handlers layer ---

const runBash = (cmd: string) =>
  Command.make("sh", "-c", cmd).pipe(
    Command.string,
    Effect.timeout("30 seconds"),
    Effect.catchAll(() => Effect.succeed(""))
  );

const ToolHandlers = NanocodeTools.toLayer(
  Effect.gen(function* () {
    const fs = yield* FileSystem.FileSystem;

    return {
      read: ({ path }) =>
        fs.readFileString(path).pipe(
          Effect.map((content) => content.split("\n").map((l, i) => `${i + 1}| ${l}`).join("\n")),
          Effect.catchAll((e) => Effect.succeed(`error: ${e}`))
        ),

      write: ({ path, content }) =>
        fs.writeFileString(path, content).pipe(
          Effect.map(() => "ok"),
          Effect.catchAll((e) => Effect.succeed(`error: ${e}`))
        ),

      edit: ({ path, old, new: replacement }) =>
        Effect.gen(function* () {
          const content = yield* fs.readFileString(path);
          if (!content.includes(old)) return "error: not found";
          yield* fs.writeFileString(path, content.replace(old, replacement));
          return "ok";
        }).pipe(Effect.catchAll((e) => Effect.succeed(`error: ${e}`))),

      bash: ({ cmd }) => runBash(cmd),

      glob: ({ pat }) => runBash(`find . -name '${pat}' -type f 2>/dev/null | head -50`),

      grep: ({ pat }) =>
        runBash(`grep -rn '${pat}' . 2>/dev/null | head -50`).pipe(
          Effect.map((r) => r || "none")
        ),
    };
  })
);

// --- Message types ---

type Message = { role: string; content: unknown };

// --- REPL ---

const readLine = Effect.gen(function* () {
  const terminal = yield* Terminal.Terminal;
  yield* terminal.display(`${B}${BL}❯${R} `);
  return yield* terminal.readLine;
});

const agentLoop = (
  messagesRef: Ref.Ref<Array<Message>>,
  model: string
): Effect.Effect<void, unknown, LanguageModel.LanguageModel | Tool.Handler<"read"> | Tool.Handler<"write"> | Tool.Handler<"edit"> | Tool.Handler<"bash"> | Tool.Handler<"glob"> | Tool.Handler<"grep">> =>
  Effect.gen(function* () {
    const messages = yield* Ref.get(messagesRef);

    // Build prompt from messages
    const prompt = messages.map((m) => 
      typeof m.content === "string" ? m.content : JSON.stringify(m.content)
    ).join("\n");

    const response = yield* LanguageModel.generateText({
      prompt,
      toolkit: NanocodeTools,
    });

    // Display text response
    if (response.text) {
      yield* Console.log(`\n${C}⏺${R} ${response.text}`);
    }

    // Display tool calls
    for (const part of response.parts) {
      if (part._tag === "ToolCall") {
        yield* Console.log(`\n${G}⏺ ${part.name}${R}`);
        const result = response.toolResults.find((r) => r.callId === part.id);
        if (result) {
          const preview = String(result._tag === "Success" ? result.value : result.cause).split("\n")[0].slice(0, 60);
          yield* Console.log(`  ${D}⎿ ${preview}${R}`);
        }
      }
    }

    // Update messages
    yield* Ref.update(messagesRef, (msgs) => [...msgs, { role: "assistant", content: response.text }]);

    // Continue if there were tool calls
    const hasToolCalls = response.parts.some((p) => p._tag === "ToolCall");
    if (hasToolCalls) {
      const toolResultsContent = response.toolResults.map((r) => ({
        type: "tool_result",
        tool_use_id: r.callId,
        content: String(r._tag === "Success" ? r.value : r.cause),
      }));
      yield* Ref.update(messagesRef, (msgs) => [...msgs, { role: "user", content: toolResultsContent }]);
      yield* agentLoop(messagesRef, model);
    }
  });

const repl = (
  messagesRef: Ref.Ref<Array<Message>>,
  model: string
): Effect.Effect<void, unknown, Terminal.Terminal | LanguageModel.LanguageModel | Tool.Handler<"read"> | Tool.Handler<"write"> | Tool.Handler<"edit"> | Tool.Handler<"bash"> | Tool.Handler<"glob"> | Tool.Handler<"grep">> =>
  Effect.gen(function* () {
    const input = yield* readLine;

    if (!input.trim()) {
      yield* repl(messagesRef, model);
      return;
    }
    if (input === "/q") return;
    if (input === "/c") {
      yield* Ref.set(messagesRef, []);
      yield* Console.log(`${G}⏺ Cleared${R}`);
      yield* repl(messagesRef, model);
      return;
    }

    yield* Ref.update(messagesRef, (msgs) => [...msgs, { role: "user", content: input }]);
    yield* agentLoop(messagesRef, model);
    yield* Console.log("");
    yield* repl(messagesRef, model);
  });

// --- Main ---

const program = Effect.gen(function* () {
  const { model } = yield* Config.unwrap(AppConfig);
  yield* Console.log(`${B}nanocode${R} | ${D}Effect AI + ${model}${R}\n`);
  const messagesRef = yield* Ref.make<Array<Message>>([]);

  // Get the model layer and provide it
  const Claude = AnthropicLanguageModel.model(model);
  
  yield* pipe(
    repl(messagesRef, model),
    Effect.provide(Claude)
  );
});

// --- Layers ---

const AnthropicLive = AnthropicClient.layerConfig({
  apiKey: Config.redacted("ANTHROPIC_API_KEY"),
}).pipe(Layer.provide(NodeHttpClient.layer));

const MainLive = Layer.mergeAll(
  NodeFileSystem.layer,
  NodeCommandExecutor.layer,
  NodeTerminal.layer,
  AnthropicLive,
  ToolHandlers.pipe(Layer.provide(NodeFileSystem.layer), Layer.provide(NodeCommandExecutor.layer))
);

pipe(
  program,
  Effect.provide(MainLive),
  NodeRuntime.runMain
);
