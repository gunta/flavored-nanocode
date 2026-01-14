#!/usr/bin/env swift
// nanocode - minimal claude code alternative (Swift)
import Foundation

let KEY = ProcessInfo.processInfo.environment["ANTHROPIC_API_KEY"]!
let MODEL = ProcessInfo.processInfo.environment["MODEL"] ?? "claude-sonnet-4-20250514"
let (R,B,D,C,G,BL) = ("\u{1b}[0m","\u{1b}[1m","\u{1b}[2m","\u{1b}[36m","\u{1b}[32m","\u{1b}[34m")

func tool(_ name: String, _ input: [String:Any]) -> String {
    switch name {
    case "read":
        guard let p = input["path"] as? String, let c = try? String(contentsOfFile: p) else { return "error" }
        return c.components(separatedBy: "\n").enumerated().map { "\($0.offset+1)| \($0.element)" }.joined(separator: "\n")
    case "write":
        guard let p = input["path"] as? String, let c = input["content"] as? String else { return "error" }
        try? c.write(toFile: p, atomically: true, encoding: .utf8); return "ok"
    case "edit":
        guard let p = input["path"] as? String, let o = input["old"] as? String, let n = input["new"] as? String,
              var t = try? String(contentsOfFile: p), t.contains(o) else { return "error: not found" }
        t = t.replacingOccurrences(of: o, with: n); try? t.write(toFile: p, atomically: true, encoding: .utf8); return "ok"
    case "glob":
        let pat = input["pat"] as? String ?? "*"
        return (try? Process.run(URL(fileURLWithPath: "/bin/sh"), arguments: ["-c", "find . -name '\(pat)' | head -50"]).standardOutput as? String) ?? "none"
    case "grep":
        let pat = input["pat"] as? String ?? ""
        return shell("grep -rn '\(pat)' . | head -50")
    case "bash":
        return shell(input["cmd"] as? String ?? "")
    default: return "unknown"
    }
}

func shell(_ cmd: String) -> String {
    let p = Process(); p.executableURL = URL(fileURLWithPath: "/bin/sh"); p.arguments = ["-c", cmd]
    let pipe = Pipe(); p.standardOutput = pipe; p.standardError = pipe
    try? p.run(); p.waitUntilExit()
    return String(data: pipe.fileHandleForReading.readDataToEndOfFile(), encoding: .utf8) ?? ""
}

let schema = """
[{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},
{"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},
{"name":"edit","description":"Edit","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"}},"required":["path","old","new"]}},
{"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
{"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
{"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}]
"""

func ask(_ messages: [[String:Any]]) -> [String:Any] {
    let body: [String:Any] = ["model": MODEL, "max_tokens": 4096, "system": "Concise assistant", "messages": messages, "tools": try! JSONSerialization.jsonObject(with: schema.data(using: .utf8)!)]
    var req = URLRequest(url: URL(string: "https://api.anthropic.com/v1/messages")!)
    req.httpMethod = "POST"
    req.setValue("application/json", forHTTPHeaderField: "Content-Type")
    req.setValue("2023-06-01", forHTTPHeaderField: "anthropic-version")
    req.setValue(KEY, forHTTPHeaderField: "x-api-key")
    req.httpBody = try? JSONSerialization.data(withJSONObject: body)
    let sem = DispatchSemaphore(value: 0)
    var result: [String:Any] = [:]
    URLSession.shared.dataTask(with: req) { data, _, _ in
        if let d = data { result = (try? JSONSerialization.jsonObject(with: d) as? [String:Any]) ?? [:] }
        sem.signal()
    }.resume()
    sem.wait()
    return result
}

print("\(B)nanocode\(R) | \(D)\(MODEL)\(R)\n")
var messages: [[String:Any]] = []

while true {
    print("\(B)\(BL)❯\(R) ", terminator: ""); fflush(stdout)
    guard let input = readLine()?.trimmingCharacters(in: .whitespaces), !input.isEmpty else { continue }
    if input == "/q" { break }
    if input == "/c" { messages = []; print("\(G)⏺ Cleared\(R)"); continue }
    
    messages.append(["role": "user", "content": input])
    
    while true {
        let resp = ask(messages)
        guard let content = resp["content"] as? [[String:Any]] else { break }
        var results: [[String:Any]] = []
        
        for block in content {
            if block["type"] as? String == "text" { print("\n\(C)⏺\(R) \(block["text"] ?? "")") }
            if block["type"] as? String == "tool_use" {
                let name = block["name"] as! String
                print("\n\(G)⏺ \(name)\(R)")
                let result = tool(name, block["input"] as? [String:Any] ?? [:])
                print("  \(D)⎿ \(result.components(separatedBy: "\n").first ?? "")\(R)")
                results.append(["type": "tool_result", "tool_use_id": block["id"]!, "content": result])
            }
        }
        messages.append(["role": "assistant", "content": content])
        if results.isEmpty { break }
        messages.append(["role": "user", "content": results])
    }
    print()
}
