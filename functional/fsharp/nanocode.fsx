#!/usr/bin/env dotnet fsi
// nanocode - minimal claude code alternative (F#)
#r "nuget: FSharp.Data"
open System
open System.IO
open System.Net.Http
open FSharp.Data

let KEY = Environment.GetEnvironmentVariable "ANTHROPIC_API_KEY"
let MODEL = Environment.GetEnvironmentVariable "MODEL" |> Option.ofObj |> Option.defaultValue "claude-sonnet-4-20250514"
let R, B, D, C, G, BL = "\x1b[0m", "\x1b[1m", "\x1b[2m", "\x1b[36m", "\x1b[32m", "\x1b[34m"

let tool name (input: JsonValue) =
    match name with
    | "read" -> File.ReadAllLines(input.["path"].AsString()) |> Array.mapi (fun i l -> $"{i+1}| {l}") |> String.concat "\n"
    | "write" -> File.WriteAllText(input.["path"].AsString(), input.["content"].AsString()); "ok"
    | "edit" -> let t = File.ReadAllText(input.["path"].AsString()) in if t.Contains(input.["old"].AsString()) then File.WriteAllText(input.["path"].AsString(), t.Replace(input.["old"].AsString(), input.["new"].AsString())); "ok" else "error: not found"
    | "glob" -> System.Diagnostics.Process.Start("sh", $"-c \"find . -name '{input.["pat"].AsString()}' | head -50\"").StandardOutput.ReadToEnd()
    | "grep" -> System.Diagnostics.Process.Start("sh", $"-c \"grep -rn '{input.["pat"].AsString()}' . | head -50\"").StandardOutput.ReadToEnd()
    | "bash" -> let p = System.Diagnostics.Process.Start(System.Diagnostics.ProcessStartInfo("sh", $"-c \"{input.["cmd"].AsString()}\"", RedirectStandardOutput=true)) in p.StandardOutput.ReadToEnd()
    | _ -> "unknown"

let schema = """[{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},{"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},{"name":"edit","description":"Edit","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"}},"required":["path","old","new"]}},{"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},{"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},{"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}]"""

let ask (messages: ResizeArray<JsonValue>) =
    use http = new HttpClient()
    http.DefaultRequestHeaders.Add("anthropic-version", "2023-06-01")
    http.DefaultRequestHeaders.Add("x-api-key", KEY)
    let body = JsonValue.Record [| "model", JsonValue.String MODEL; "max_tokens", JsonValue.Number 4096m; "system", JsonValue.String "Concise assistant"; "messages", JsonValue.Array (messages.ToArray()); "tools", JsonValue.Parse schema |]
    let resp = http.PostAsync("https://api.anthropic.com/v1/messages", new StringContent(body.ToString(), Text.Encoding.UTF8, "application/json")).Result
    JsonValue.Parse(resp.Content.ReadAsStringAsync().Result)

printfn $"{B}nanocode{R} | {D}{MODEL}{R}\n"
let messages = ResizeArray<JsonValue>()

while true do
    printf $"{B}{BL}❯{R} "
    match Console.ReadLine() with
    | null | "/q" -> exit 0
    | "" -> ()
    | "/c" -> messages.Clear(); printfn $"{G}⏺ Cleared{R}"
    | input ->
        messages.Add(JsonValue.Record [| "role", JsonValue.String "user"; "content", JsonValue.String input |])
        let rec loop () =
            let resp = ask messages
            let content = resp.["content"].AsArray()
            let results = ResizeArray<JsonValue>()
            for block in content do
                match block.["type"].AsString() with
                | "text" -> printfn $"\n{C}⏺{R} {block.["text"].AsString()}"
                | "tool_use" ->
                    let name = block.["name"].AsString()
                    printfn $"\n{G}⏺ {name}{R}"
                    let result = tool name block.["input"]
                    printfn $"  {D}⎿ {result.Split('\n').[0]}{R}"
                    results.Add(JsonValue.Record [| "type", JsonValue.String "tool_result"; "tool_use_id", block.["id"]; "content", JsonValue.String result |])
                | _ -> ()
            messages.Add(JsonValue.Record [| "role", JsonValue.String "assistant"; "content", JsonValue.Array content |])
            if results.Count > 0 then
                messages.Add(JsonValue.Record [| "role", JsonValue.String "user"; "content", JsonValue.Array (results.ToArray()) |])
                loop ()
        loop (); printfn ""
