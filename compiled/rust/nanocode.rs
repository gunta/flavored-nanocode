#!/usr/bin/env rust-script
//! nanocode - minimal claude code alternative (Rust)
//! ```cargo
//! [dependencies]
//! ureq = { version = "2", features = ["json"] }
//! serde = { version = "1", features = ["derive"] }
//! serde_json = "1"
//! ```
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::{env, fs, io::{self, BufRead, Write}, process::Command};

const R: &str = "\x1b[0m"; const B: &str = "\x1b[1m"; const D: &str = "\x1b[2m";
const C: &str = "\x1b[36m"; const G: &str = "\x1b[32m"; const BL: &str = "\x1b[34m";

#[derive(Serialize, Deserialize)] struct Msg { role: String, content: Value }

fn run_tool(name: &str, input: &Value) -> String {
    match name {
        "read" => fs::read_to_string(input["path"].as_str().unwrap_or(""))
            .map(|c| c.lines().enumerate().map(|(i,l)| format!("{}| {}", i+1, l)).collect::<Vec<_>>().join("\n"))
            .unwrap_or_else(|e| format!("error: {}", e)),
        "write" => fs::write(input["path"].as_str().unwrap_or(""), input["content"].as_str().unwrap_or(""))
            .map(|_| "ok".into()).unwrap_or_else(|e| format!("error: {}", e)),
        "edit" => {
            let path = input["path"].as_str().unwrap_or("");
            let (old, new) = (input["old"].as_str().unwrap_or(""), input["new"].as_str().unwrap_or(""));
            fs::read_to_string(path).map(|t| {
                if !t.contains(old) { return "error: not found".into(); }
                fs::write(path, t.replacen(old, new, 1)).map(|_| "ok".into()).unwrap_or_else(|e| format!("error: {}", e))
            }).unwrap_or_else(|e| format!("error: {}", e))
        }
        "glob" => {
            let pat = input["pat"].as_str().unwrap_or("*");
            glob::glob(&format!("**/{}", pat)).map(|p| p.filter_map(Result::ok).take(50)
                .map(|p| p.display().to_string()).collect::<Vec<_>>().join("\n"))
                .unwrap_or_else(|_| "none".into())
        }
        "grep" => Command::new("grep").args(["-rn", input["pat"].as_str().unwrap_or(""), "."])
            .output().map(|o| String::from_utf8_lossy(&o.stdout).lines().take(50).collect::<Vec<_>>().join("\n"))
            .unwrap_or_else(|_| "none".into()),
        "bash" => Command::new("sh").args(["-c", input["cmd"].as_str().unwrap_or("")])
            .output().map(|o| String::from_utf8_lossy(&o.stdout).into()).unwrap_or_else(|e| format!("error: {}", e)),
        _ => "unknown tool".into()
    }
}

fn main() {
    let key = env::var("ANTHROPIC_API_KEY").expect("ANTHROPIC_API_KEY required");
    let model = env::var("MODEL").unwrap_or("claude-sonnet-4-20250514".into());
    let api = env::var("API_URL").unwrap_or("https://api.anthropic.com/v1/messages".into());
    
    println!("{B}nanocode{R} | {D}{model}{R}\n");
    let schema = json!([
        {"name":"read","description":"Read file","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},
        {"name":"write","description":"Write file","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},
        {"name":"edit","description":"Edit file","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"}},"required":["path","old","new"]}},
        {"name":"glob","description":"Find files","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
        {"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
        {"name":"bash","description":"Run command","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}
    ]);
    
    let mut messages: Vec<Value> = vec![];
    let stdin = io::stdin();
    
    loop {
        print!("{B}{BL}❯{R} "); io::stdout().flush().ok();
        let mut input = String::new();
        if stdin.lock().read_line(&mut input).is_err() { break; }
        let input = input.trim();
        if input.is_empty() { continue; }
        if input == "/q" { break; }
        if input == "/c" { messages.clear(); println!("{G}⏺ Cleared{R}"); continue; }
        
        messages.push(json!({"role": "user", "content": input}));
        
        loop {
            let body = json!({"model": model, "max_tokens": 4096, "system": "Concise coding assistant", "messages": messages, "tools": schema});
            let resp: Value = ureq::post(api)
                .set("Content-Type", "application/json")
                .set("anthropic-version", "2023-06-01")
                .set("x-api-key", &key)
                .send_json(&body).unwrap().into_json().unwrap();
            
            let content = resp["content"].as_array().unwrap();
            let mut results = vec![];
            
            for block in content {
                if block["type"] == "text" { println!("\n{C}⏺{R} {}", block["text"].as_str().unwrap_or("")); }
                if block["type"] == "tool_use" {
                    let name = block["name"].as_str().unwrap();
                    println!("\n{G}⏺ {name}{R}({D}...{R})");
                    let result = run_tool(name, &block["input"]);
                    println!("  {D}⎿ {}{R}", result.lines().next().unwrap_or(""));
                    results.push(json!({"type": "tool_result", "tool_use_id": block["id"], "content": result}));
                }
            }
            messages.push(json!({"role": "assistant", "content": content}));
            if results.is_empty() { break; }
            messages.push(json!({"role": "user", "content": results}));
        }
        println!();
    }
}
