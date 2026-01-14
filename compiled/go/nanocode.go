//go:build ignore

// nanocode - minimal claude code alternative (Go)
package main

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strings"
)

const (
	R, B, D, C, G, BL = "\033[0m", "\033[1m", "\033[2m", "\033[36m", "\033[32m", "\033[34m"
)

var schema = json.RawMessage(`[
	{"name":"read","description":"Read file","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},
	{"name":"write","description":"Write file","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},
	{"name":"edit","description":"Edit file","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"}},"required":["path","old","new"]}},
	{"name":"glob","description":"Find files","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
	{"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
	{"name":"bash","description":"Run command","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}
]`)

func runTool(name string, input map[string]any) string {
	switch name {
	case "read":
		b, err := os.ReadFile(input["path"].(string))
		if err != nil {
			return "error: " + err.Error()
		}
		var out []string
		for i, l := range strings.Split(string(b), "\n") {
			out = append(out, fmt.Sprintf("%d| %s", i+1, l))
		}
		return strings.Join(out, "\n")
	case "write":
		if err := os.WriteFile(input["path"].(string), []byte(input["content"].(string)), 0644); err != nil {
			return "error: " + err.Error()
		}
		return "ok"
	case "edit":
		b, _ := os.ReadFile(input["path"].(string))
		t := string(b)
		if !strings.Contains(t, input["old"].(string)) {
			return "error: not found"
		}
		os.WriteFile(input["path"].(string), []byte(strings.Replace(t, input["old"].(string), input["new"].(string), 1)), 0644)
		return "ok"
	case "glob":
		var files []string
		filepath.Walk(".", func(p string, _ os.FileInfo, _ error) error {
			if strings.Contains(p, strings.Trim(input["pat"].(string), "*")) {
				files = append(files, p)
			}
			return nil
		})
		if len(files) > 50 {
			files = files[:50]
		}
		return strings.Join(files, "\n")
	case "grep":
		re := regexp.MustCompile(input["pat"].(string))
		var hits []string
		filepath.Walk(".", func(p string, info os.FileInfo, _ error) error {
			if info == nil || info.IsDir() {
				return nil
			}
			b, _ := os.ReadFile(p)
			for i, l := range strings.Split(string(b), "\n") {
				if re.MatchString(l) {
					hits = append(hits, fmt.Sprintf("%s:%d:%s", p, i+1, l))
				}
			}
			return nil
		})
		if len(hits) > 50 {
			hits = hits[:50]
		}
		return strings.Join(hits, "\n")
	case "bash":
		out, _ := exec.Command("sh", "-c", input["cmd"].(string)).CombinedOutput()
		return string(out)
	}
	return "unknown"
}

func main() {
	key := os.Getenv("ANTHROPIC_API_KEY")
	model := os.Getenv("MODEL")
	if model == "" {
		model = "claude-sonnet-4-20250514"
	}
	fmt.Printf("%snanocode%s | %s%s%s\n\n", B, R, D, model, R)

	var messages []map[string]any
	scanner := bufio.NewScanner(os.Stdin)

	for {
		fmt.Printf("%s%s❯%s ", B, BL, R)
		if !scanner.Scan() {
			break
		}
		input := strings.TrimSpace(scanner.Text())
		if input == "" {
			continue
		}
		if input == "/q" {
			break
		}
		if input == "/c" {
			messages = nil
			fmt.Printf("%s⏺ Cleared%s\n", G, R)
			continue
		}

		messages = append(messages, map[string]any{"role": "user", "content": input})

		for {
			body, _ := json.Marshal(map[string]any{"model": model, "max_tokens": 4096, "system": "Concise coding assistant", "messages": messages, "tools": schema})
			req, _ := http.NewRequest("POST", "https://api.anthropic.com/v1/messages", bytes.NewReader(body))
			req.Header.Set("Content-Type", "application/json")
			req.Header.Set("anthropic-version", "2023-06-01")
			req.Header.Set("x-api-key", key)
			resp, _ := http.DefaultClient.Do(req)
			respBody, _ := io.ReadAll(resp.Body)
			resp.Body.Close()

			var data struct{ Content []map[string]any }
			json.Unmarshal(respBody, &data)

			var results []map[string]any
			for _, block := range data.Content {
				if block["type"] == "text" {
					fmt.Printf("\n%s⏺%s %s", C, R, block["text"])
				}
				if block["type"] == "tool_use" {
					name := block["name"].(string)
					fmt.Printf("\n%s⏺ %s%s\n", G, name, R)
					result := runTool(name, block["input"].(map[string]any))
					fmt.Printf("  %s⎿ %s%s\n", D, strings.Split(result, "\n")[0], R)
					results = append(results, map[string]any{"type": "tool_result", "tool_use_id": block["id"], "content": result})
				}
			}
			messages = append(messages, map[string]any{"role": "assistant", "content": data.Content})
			if len(results) == 0 {
				break
			}
			messages = append(messages, map[string]any{"role": "user", "content": results})
		}
		fmt.Println()
	}
}
