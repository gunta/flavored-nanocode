// nanocode - minimal claude code alternative (C#)
// dotnet run
using System.Text; using System.Text.Json;

var KEY = Environment.GetEnvironmentVariable("ANTHROPIC_API_KEY")!;
var MODEL = Environment.GetEnvironmentVariable("MODEL") ?? "claude-sonnet-4-20250514";
var (R,B,D,C,G,BL) = ("\x1b[0m","\x1b[1m","\x1b[2m","\x1b[36m","\x1b[32m","\x1b[34m");

string Tool(string name, JsonElement input) => name switch {
    "read" => string.Join("\n", File.ReadAllLines(input.GetProperty("path").GetString()!).Select((l,i) => $"{i+1}| {l}")),
    "write" => (File.WriteAllText(input.GetProperty("path").GetString()!, input.GetProperty("content").GetString()), "ok").Item2,
    "edit" => Edit(input.GetProperty("path").GetString()!, input.GetProperty("old").GetString()!, input.GetProperty("new").GetString()!),
    "glob" => RunCmd($"find . -name '{input.GetProperty("pat").GetString()}' | head -50"),
    "grep" => RunCmd($"grep -rn '{input.GetProperty("pat").GetString()}' . | head -50"),
    "bash" => RunCmd(input.GetProperty("cmd").GetString()!),
    _ => "unknown"
};

string Edit(string p, string o, string n) { var t = File.ReadAllText(p); if (!t.Contains(o)) return "error: not found"; File.WriteAllText(p, t.Replace(o, n)); return "ok"; }
string RunCmd(string cmd) { var p = System.Diagnostics.Process.Start(new System.Diagnostics.ProcessStartInfo("sh", $"-c \"{cmd}\"") { RedirectStandardOutput = true })!; return p.StandardOutput.ReadToEnd(); }

var schema = """[{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},{"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},{"name":"edit","description":"Edit","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"}},"required":["path","old","new"]}},{"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},{"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},{"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}]""";

async Task<JsonDocument> Ask(List<object> msgs) {
    using var http = new HttpClient();
    http.DefaultRequestHeaders.Add("anthropic-version", "2023-06-01");
    http.DefaultRequestHeaders.Add("x-api-key", KEY);
    var body = JsonSerializer.Serialize(new { model = MODEL, max_tokens = 4096, system = "Concise assistant", messages = msgs, tools = JsonDocument.Parse(schema).RootElement });
    var resp = await http.PostAsync("https://api.anthropic.com/v1/messages", new StringContent(body, Encoding.UTF8, "application/json"));
    return JsonDocument.Parse(await resp.Content.ReadAsStringAsync());
}

Console.WriteLine($"{B}nanocode{R} | {D}{MODEL}{R}\n");
var messages = new List<object>();

while (true) {
    Console.Write($"{B}{BL}❯{R} ");
    var input = Console.ReadLine()?.Trim();
    if (string.IsNullOrEmpty(input)) continue;
    if (input == "/q") break;
    if (input == "/c") { messages.Clear(); Console.WriteLine($"{G}⏺ Cleared{R}"); continue; }
    
    messages.Add(new { role = "user", content = input });
    
    while (true) {
        var resp = await Ask(messages);
        var content = resp.RootElement.GetProperty("content").EnumerateArray().ToList();
        var results = new List<object>();
        
        foreach (var block in content) {
            if (block.GetProperty("type").GetString() == "text") Console.WriteLine($"\n{C}⏺{R} {block.GetProperty("text").GetString()}");
            if (block.GetProperty("type").GetString() == "tool_use") {
                var name = block.GetProperty("name").GetString()!;
                Console.WriteLine($"\n{G}⏺ {name}{R}");
                var result = Tool(name, block.GetProperty("input"));
                Console.WriteLine($"  {D}⎿ {result.Split('\n')[0]}{R}");
                results.Add(new { type = "tool_result", tool_use_id = block.GetProperty("id").GetString(), content = result });
            }
        }
        messages.Add(new { role = "assistant", content = content.Select(b => JsonSerializer.Deserialize<object>(b.GetRawText())) });
        if (!results.Any()) break;
        messages.Add(new { role = "user", content = results });
    }
    Console.WriteLine();
}
