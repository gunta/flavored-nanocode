// nanocode - minimal claude code alternative (Go Fiber web UI)
// go get github.com/gofiber/fiber/v2 && go run nanocode.go
package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"github.com/gofiber/fiber/v2"
)

var (
	KEY      = func() string { if v := os.Getenv("OPENROUTER_API_KEY"); v != "" { return v }; return os.Getenv("ANTHROPIC_API_KEY") }()
	API      = func() string { if os.Getenv("OPENROUTER_API_KEY") != "" { return "https://openrouter.ai/api/v1/messages" }; return "https://api.anthropic.com/v1/messages" }()
	MODEL    = func() string { if v := os.Getenv("MODEL"); v != "" { return v }; if os.Getenv("OPENROUTER_API_KEY") != "" { return "anthropic/claude-opus-4-5" }; return "claude-opus-4-5" }()
	messages []map[string]any
)

func getEnv(k, d string) string { if v := os.Getenv(k); v != "" { return v }; return d }

func tool(name string, input map[string]any) string {
	switch name {
	case "read":
		b, _ := os.ReadFile(input["path"].(string))
		off := 0
		if v, ok := input["offset"].(float64); ok { off = int(v) }
		lim := -1
		if v, ok := input["limit"].(float64); ok { lim = int(v) }
		var lines []string
		for i, l := range strings.Split(string(b), "\n") {
			if i < off { continue }
			if lim >= 0 && len(lines) >= lim { break }
			lines = append(lines, fmt.Sprintf("%d| %s", i+1, l))
		}
		return strings.Join(lines, "\n")
	case "write":
		os.WriteFile(input["path"].(string), []byte(input["content"].(string)), 0644)
		return "ok"
	case "edit":
		path := input["path"].(string)
		old := input["old"].(string)
		newv := input["new"].(string)
		all, _ := input["all"].(bool)
		b, _ := os.ReadFile(path)
		txt := string(b)
		cnt := strings.Count(txt, old)
		if cnt == 0 { return "error: old_string not found" }
		if cnt > 1 && !all { return fmt.Sprintf("error: old_string appears %d times, use all=true", cnt) }
		if all { txt = strings.ReplaceAll(txt, old, newv) } else { txt = strings.Replace(txt, old, newv, 1) }
		os.WriteFile(path, []byte(txt), 0644)
		return "ok"
	case "glob":
		base := "."
		if v, ok := input["path"].(string); ok && v != "" { base = v }
		pat := input["pat"].(string)
		var matches []string
		filepath.Walk(base, func(path string, info os.FileInfo, err error) error {
			if err == nil && strings.Contains(path, pat) { matches = append(matches, path) }
			if len(matches) >= 50 { return filepath.SkipDir }
			return nil
		})
		return strings.Join(matches, "\n")
	case "grep":
		base := "."
		if v, ok := input["path"].(string); ok && v != "" { base = v }
		pat := input["pat"].(string)
		var hits []string
		filepath.Walk(base, func(path string, info os.FileInfo, err error) error {
			if err != nil || info.IsDir() { return nil }
			data, err := os.ReadFile(path); if err != nil { return nil }
			for i, l := range strings.Split(string(data), "\n") {
				if strings.Contains(l, pat) { hits = append(hits, fmt.Sprintf("%s:%d:%s", path, i+1, l)) }
				if len(hits) >= 50 { return filepath.SkipDir }
			}
			return nil
		})
		if len(hits) == 0 { return "none" }
		return strings.Join(hits, "\n")
	case "bash":
		out, _ := exec.Command("sh", "-c", input["cmd"].(string)).CombinedOutput()
		return string(out)
	}
	return "unknown"
}

var schema = json.RawMessage(`[
  {"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"},"offset":{"type":"integer"},"limit":{"type":"integer"}},"required":["path"]}},
  {"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},
  {"name":"edit","description":"Replace","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"},"all":{"type":"boolean"}},"required":["path","old","new"]}},
  {"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"},"path":{"type":"string"}},"required":["pat"]}},
  {"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"},"path":{"type":"string"}},"required":["pat"]}},
  {"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}
]`)

func ask() map[string]any {
	body, _ := json.Marshal(map[string]any{"model": MODEL, "max_tokens": 8192, "system": "Concise assistant", "messages": messages, "tools": schema})
	req, _ := http.NewRequest("POST", API, bytes.NewReader(body))
	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("anthropic-version", "2023-06-01")
	if os.Getenv("OPENROUTER_API_KEY") != "" {
		req.Header.Set("Authorization", "Bearer "+KEY)
	} else {
		req.Header.Set("x-api-key", KEY)
	}
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
