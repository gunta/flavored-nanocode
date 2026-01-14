// nanocode - minimal claude code alternative (Rust Axum web UI)
// cargo add axum tokio serde serde_json reqwest && cargo run
use axum::{extract::Json, response::Html, routing::{get, post}, Router};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::{env, fs, process::Command, sync::Mutex};
use tokio::sync::OnceCell;

static MESSAGES: OnceCell<Mutex<Vec<Value>>> = OnceCell::const_new();

fn tool(name: &str, input: &Value) -> String {
    match name {
        "read" => fs::read_to_string(input["path"].as_str().unwrap_or(""))
            .map(|c| c.lines().enumerate().map(|(i, l)| format!("{}| {}", i + 1, l)).collect::<Vec<_>>().join("\n"))
            .unwrap_or_else(|e| format!("error: {}", e)),
        "write" => fs::write(input["path"].as_str().unwrap_or(""), input["content"].as_str().unwrap_or(""))
            .map(|_| "ok".into()).unwrap_or_else(|e| format!("error: {}", e)),
        "bash" => Command::new("sh").args(["-c", input["cmd"].as_str().unwrap_or("")])
            .output().map(|o| String::from_utf8_lossy(&o.stdout).into()).unwrap_or_else(|e| format!("error: {}", e)),
        _ => "unknown".into()
    }
}

async fn ask(messages: &[Value]) -> Value {
    let key = env::var("ANTHROPIC_API_KEY").unwrap();
    let model = env::var("MODEL").unwrap_or("claude-sonnet-4-20250514".into());
    let schema = json!([{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},
        {"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},
        {"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}]);
    reqwest::Client::new().post("https://api.anthropic.com/v1/messages")
        .header("Content-Type", "application/json").header("anthropic-version", "2023-06-01").header("x-api-key", key)
        .json(&json!({"model": model, "max_tokens": 4096, "system": "Concise assistant", "messages": messages, "tools": schema}))
        .send().await.unwrap().json().await.unwrap()
}

#[derive(Deserialize)] struct Msg { msg: String }
#[derive(Serialize)] struct Out { r#type: String, text: Option<String>, name: Option<String> }

async fn index() -> Html<&'static str> { Html(r#"<!DOCTYPE html><html><head><title>nanocode</title>
<style>body{font:14px monospace;background:#0d1117;color:#c9d1d9;max-width:800px;margin:0 auto;padding:20px}
#out{white-space:pre-wrap}form{display:flex}input{flex:1;background:#21262d;border:1px solid #30363d;color:#c9d1d9;padding:8px}
button{background:#238636;border:none;color:#fff;padding:8px 16px}</style></head>
<body><h2>🧬 nanocode (Axum)</h2><div id="out"></div>
<form onsubmit="send(event)"><input id="i" autofocus><button>→</button></form>
<script>async function send(e){e.preventDefault();const m=i.value;i.value='';out.innerHTML+='❯ '+m+'\n';
const r=await(await fetch('/chat',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({msg:m})})).json();
out.innerHTML+=r.map(b=>b.type==='text'?'⏺ '+b.text:'⚙ '+b.name).join('\n')+'\n\n';}</script></body></html>"#) }

async fn chat(Json(body): Json<Msg>) -> Json<Vec<Out>> {
    let msgs = MESSAGES.get_or_init(|| async { Mutex::new(vec![]) }).await;
    if body.msg == "/c" { msgs.lock().unwrap().clear(); return Json(vec![Out { r#type: "text".into(), text: Some("Cleared".into()), name: None }]); }
    msgs.lock().unwrap().push(json!({"role": "user", "content": body.msg}));
    let mut out = vec![];
    loop {
        let resp = ask(&msgs.lock().unwrap()).await;
        let content = resp["content"].as_array().unwrap();
        for block in content {
            if block["type"] == "text" { out.push(Out { r#type: "text".into(), text: Some(block["text"].as_str().unwrap().into()), name: None }); }
            if block["type"] == "tool_use" {
                let name = block["name"].as_str().unwrap();
                out.push(Out { r#type: "tool".into(), text: None, name: Some(name.into()) });
                let result = tool(name, &block["input"]);
                let mut m = msgs.lock().unwrap();
                m.push(json!({"role": "assistant", "content": content}));
                m.push(json!({"role": "user", "content": [{"type": "tool_result", "tool_use_id": block["id"], "content": result}]}));
            }
        }
        if !content.iter().any(|b| b["type"] == "tool_use") { msgs.lock().unwrap().push(json!({"role": "assistant", "content": content})); break; }
    }
    Json(out)
}

#[tokio::main]
async fn main() {
    println!("nanocode running on http://localhost:3000");
    let app = Router::new().route("/", get(index)).route("/chat", post(chat));
    axum::serve(tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap(), app).await.unwrap();
}
