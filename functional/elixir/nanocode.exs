#!/usr/bin/env elixir
# nanocode - minimal claude code alternative (Elixir)
Mix.install([{:req, "~> 0.4"}])

defmodule Nano do
  @key System.get_env("ANTHROPIC_API_KEY")
  @model System.get_env("MODEL") || "claude-sonnet-4-20250514"
  @r "\e[0m"; @b "\e[1m"; @d "\e[2m"; @c "\e[36m"; @g "\e[32m"; @bl "\e[34m"

  @schema [
    %{name: "read", description: "Read", input_schema: %{type: "object", properties: %{path: %{type: "string"}}, required: ["path"]}},
    %{name: "write", description: "Write", input_schema: %{type: "object", properties: %{path: %{type: "string"}, content: %{type: "string"}}, required: ["path", "content"]}},
    %{name: "edit", description: "Edit", input_schema: %{type: "object", properties: %{path: %{type: "string"}, old: %{type: "string"}, new: %{type: "string"}}, required: ["path", "old", "new"]}},
    %{name: "glob", description: "Find", input_schema: %{type: "object", properties: %{pat: %{type: "string"}}, required: ["pat"]}},
    %{name: "grep", description: "Search", input_schema: %{type: "object", properties: %{pat: %{type: "string"}}, required: ["pat"]}},
    %{name: "bash", description: "Run", input_schema: %{type: "object", properties: %{cmd: %{type: "string"}}, required: ["cmd"]}}
  ]

  def tool("read", %{"path" => p}), do: File.read!(p) |> String.split("\n") |> Enum.with_index(1) |> Enum.map(fn {l,i} -> "#{i}| #{l}" end) |> Enum.join("\n")
  def tool("write", %{"path" => p, "content" => c}), do: (File.write!(p, c); "ok")
  def tool("edit", %{"path" => p, "old" => o, "new" => n}) do
    t = File.read!(p)
    if String.contains?(t, o), do: (File.write!(p, String.replace(t, o, n, global: false)); "ok"), else: "error: not found"
  end
  def tool("glob", %{"pat" => pat}), do: Path.wildcard("**/#{pat}") |> Enum.take(50) |> Enum.join("\n")
  def tool("grep", %{"pat" => pat}), do: System.cmd("grep", ["-rn", pat, "."]) |> elem(0) |> String.split("\n") |> Enum.take(50) |> Enum.join("\n")
  def tool("bash", %{"cmd" => cmd}), do: System.cmd("sh", ["-c", cmd]) |> elem(0)

  def ask(msgs) do
    Req.post!("https://api.anthropic.com/v1/messages",
      headers: [{"content-type", "application/json"}, {"anthropic-version", "2023-06-01"}, {"x-api-key", @key}],
      json: %{model: @model, max_tokens: 4096, system: "Concise assistant", messages: msgs, tools: @schema}
    ).body
  end

  def run(msgs \\ []) do
    IO.write("#{@b}#{@bl}❯#{@r} ")
    case IO.gets("") |> String.trim() do
      "/q" -> :ok
      "" -> run(msgs)
      "/c" -> IO.puts("#{@g}⏺ Cleared#{@r}"); run([])
      input ->
        msgs = msgs ++ [%{role: "user", content: input}]
        loop(msgs)
    end
  end

  def loop(msgs) do
    %{"content" => content} = ask(msgs)
    {msgs, results} = Enum.reduce(content, {msgs, []}, fn block, {m, r} ->
      case block["type"] do
        "text" -> IO.puts("\n#{@c}⏺#{@r} #{block["text"]}"); {m, r}
        "tool_use" ->
          IO.puts("\n#{@g}⏺ #{block["name"]}#{@r}")
          result = tool(block["name"], block["input"])
          IO.puts("  #{@d}⎿ #{result |> String.split("\n") |> hd}#{@r}")
          {m, r ++ [%{type: "tool_result", tool_use_id: block["id"], content: result}]}
        _ -> {m, r}
      end
    end)
    msgs = msgs ++ [%{role: "assistant", content: content}]
    if results == [] do IO.puts(""); run(msgs)
    else loop(msgs ++ [%{role: "user", content: results}]) end
  end
end

IO.puts("#{"\e[1m"}nanocode#{"\e[0m"} | #{"\e[2m"}#{System.get_env("MODEL") || "claude-sonnet-4-20250514"}#{"\e[0m"}\n")
Nano.run()
