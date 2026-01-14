// nanocode - minimal claude code alternative (Go Fiber web UI)
// go get github.com/gofiber/fiber/v2 && go run nanocode.go
package main

import (
	"bytes"
	"encoding/json"
	"io"
	"net/http"
	"os"
	"os/exec"
	"strings"

	"github.com/gofiber/fiber/v2"
)

var (
	KEY      = os.Getenv("ANTHROPIC_API_KEY")
	MODEL    = getEnv("MODEL", "claude-sonnet-4-20250514")
	messages []map[string]any
)

func getEnv(k, d string) string { if v := os.Getenv(k); v != "" { return v }; return d }

func tool(name string, input map[string]any) string {
	switch name {
	case "read":
		b, _ := os.ReadFile(input["path"].(string))
		var lines []string
		for i, l := range strings.Split(string(b), "\n") { lines = append(lines, string(rune(i+1))+"| "+l) }
		return strings.Join(lines, "\n")
	case "write":
		os.WriteFile(input["path"].(string), []byte(input["content"].(string)), 0644)
		return "ok"
	case "bash":
		out, _ := exec.Command("sh", "-c", input["cmd"].(string)).CombinedOutput()
		return string(out)
	}
	return "unknown"
}

var schema = json.RawMessage(`[{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},{"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},{"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}]`)

func ask() map[string]any {
	body, _ := json.Marshal(map[string]any{"model": MODEL, "max_tokens": 4096, "system": "Concise assistant", "messages": messages, "tools": schema})
	req, _ := http.NewRequest("POST", "https://api.anthropic.com/v1/messages", bytes.NewReader(body))
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("anthropic-version", "2023-06-01")
	req.Header.Set("x-api-key", KEY)
	resp, _ := http.DefaultClient.Do(req)
	respBody, _ := io.ReadAll(resp.Body)
	var data map[string]any
	json.Unmarshal(respBody, &data)
	return data
}

func main() {
	app := fiber.New()

	app.Get("/", func(c *fiber.Ctx) error {
		c.Set("Content-Type", "text/html")
		return c.SendString(`<!DOCTYPE html><html><head><title>nanocode</title>
<style>body{font:14px monospace;background:#0d1117;color:#c9d1d9;max-width:800px;margin:0 auto;padding:20px}
#out{white-space:pre-wrap}form{display:flex}input{flex:1;background:#21262d;border:1px solid #30363d;color:#c9d1d9;padding:8px}
button{background:#238636;border:none;color:#fff;padding:8px 16px}</style></head>
<body><h2>🧬 nanocode (Fiber)</h2><div id="out"></div>
<form onsubmit="send(event)"><input id="i" autofocus><button>→</button></form>
<script>async function send(e){e.preventDefault();const m=i.value;i.value='';out.innerHTML+='❯ '+m+'\n';
const r=await(await fetch('/chat',{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({msg:m})})).json();
out.innerHTML+=r.map(b=>b.type==='text'?'⏺ '+b.text:'⚙ '+b.name).join('\n')+'\n\n';}</script></body></html>`)
	})

	app.Post("/chat", func(c *fiber.Ctx) error {
		var body struct{ Msg string }
		c.BodyParser(&body)
		if body.Msg == "/c" { messages = nil; return c.JSON([]map[string]string{{"type": "text", "text": "Cleared"}}) }

		messages = append(messages, map[string]any{"role": "user", "content": body.Msg})
		var out []map[string]string

		for {
			resp := ask()
			content := resp["content"].([]any)
			for _, b := range content {
				block := b.(map[string]any)
				if block["type"] == "text" { out = append(out, map[string]string{"type": "text", "text": block["text"].(string)}) }
				if block["type"] == "tool_use" {
					name := block["name"].(string)
					out = append(out, map[string]string{"type": "tool", "name": name})
					result := tool(name, block["input"].(map[string]any))
					messages = append(messages, map[string]any{"role": "assistant", "content": content})
					messages = append(messages, map[string]any{"role": "user", "content": []map[string]any{{"type": "tool_result", "tool_use_id": block["id"], "content": result}}})
				}
			}
			hasTools := false
			for _, b := range content { if b.(map[string]any)["type"] == "tool_use" { hasTools = true } }
			if !hasTools { messages = append(messages, map[string]any{"role": "assistant", "content": content}); break }
		}
		return c.JSON(out)
	})

	println("nanocode running on http://localhost:3000")
	app.Listen(":3000")
}
