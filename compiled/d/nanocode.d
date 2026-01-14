#!/usr/bin/env dub
/+ dub.sdl:
    name "nanocode"
    dependency "vibe-d:http" version="~>0.9"
    dependency "vibe-d:data" version="~>0.9"
+/
// nanocode - minimal claude code alternative (D)
import std.stdio, std.string, std.file, std.process, std.array, std.algorithm, std.conv;
import vibe.data.json, vibe.http.client, vibe.stream.operations;

enum R = "\033[0m", B = "\033[1m", D = "\033[2m", C = "\033[36m", G = "\033[32m", BL = "\033[34m";
string KEY, MODEL;

string tool(string name, Json input) {
    try switch (name) {
        case "read": return readText(input["path"].get!string).split("\n").enumerate.map!(t => format("%d| %s", t[0]+1, t[1])).join("\n");
        case "write": std.file.write(input["path"].get!string, input["content"].get!string); return "ok";
        case "bash": return executeShell(input["cmd"].get!string).output;
        case "glob": return executeShell("find . -name '" ~ input["pat"].get!string ~ "' | head -50").output;
        case "grep": return executeShell("grep -rn '" ~ input["pat"].get!string ~ "' . | head -50").output;
        default: return "unknown";
    } catch (Exception e) return "error: " ~ e.msg;
}

Json ask(Json[] messages) {
    auto schema = parseJsonString(`[{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},{"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},{"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}},{"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},{"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}}]`);
    auto body = Json(["model": Json(MODEL), "max_tokens": Json(4096), "system": Json("Concise assistant"), "messages": Json(messages), "tools": schema]);
    Json result;
    requestHTTP("https://api.anthropic.com/v1/messages",
        (scope req) { req.method = HTTPMethod.POST;
            req.headers["Content-Type"] = "application/json"; req.headers["anthropic-version"] = "2023-06-01"; req.headers["x-api-key"] = KEY;
            req.writeBody(body.toString); },
        (scope res) { result = parseJsonString(res.bodyReader.readAllUTF8); });
    return result;
}

void main() {
    KEY = environment.get("ANTHROPIC_API_KEY");
    MODEL = environment.get("MODEL", "claude-sonnet-4-20250514");
    writefln("%snanocode%s | %s%s%s\n", B, R, D, MODEL, R);
    Json[] messages;

    while (true) {
        writef("%s%s❯%s ", B, BL, R); stdout.flush;
        auto input = readln.strip;
        if (input == "") continue;
        if (input == "/q") break;
        if (input == "/c") { messages = []; writefln("%s⏺ Cleared%s", G, R); continue; }

        messages ~= Json(["role": Json("user"), "content": Json(input)]);

        while (true) {
            auto resp = ask(messages);
            auto content = resp["content"].get!(Json[]);
            Json[] results;

            foreach (block; content) {
                if (block["type"].get!string == "text") writefln("\n%s⏺%s %s", C, R, block["text"].get!string);
                if (block["type"].get!string == "tool_use") {
                    auto name = block["name"].get!string;
                    writefln("\n%s⏺ %s%s", G, name, R);
                    auto result = tool(name, block["input"]);
                    writefln("  %s⎿ %s%s", D, result.split("\n")[0], R);
                    results ~= Json(["type": Json("tool_result"), "tool_use_id": block["id"], "content": Json(result)]);
                }
            }
            messages ~= Json(["role": Json("assistant"), "content": Json(content)]);
            if (results.length == 0) break;
            messages ~= Json(["role": Json("user"), "content": Json(results)]);
        }
        writeln;
    }
}
