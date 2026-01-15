#!/usr/bin/env crystal
# nanocode - minimal claude code alternative (Crystal)
require "http/client"
require "json"

KEY = ENV["ANTHROPIC_API_KEY"]
MODEL = ENV["MODEL"]? || "claude-sonnet-4-20250514"
API_URL = ENV["API_URL"]? || "https://api.anthropic.com/v1/messages"
R = "\e[0m"
B = "\e[1m"
D = "\e[2m"
C = "\e[36m"
G = "\e[32m"
BL = "\e[34m"

def tool(name : String, input : JSON::Any) : String
  case name
  when "read" then File.read_lines(input["path"].as_s).map_with_index { |l, i| "#{i + 1}| #{l}" }.join("\n")
  when "write" then File.write(input["path"].as_s, input["content"].as_s); "ok"
  when "edit"
    t = File.read(input["path"].as_s)
    return "error: not found" unless t.includes?(input["old"].as_s)
    File.write(input["path"].as_s, t.sub(input["old"].as_s, input["new"].as_s)); "ok"
  when "glob" then `find . -name '#{input["pat"].as_s}' | head -50`
  when "grep" then `grep -rn '#{input["pat"].as_s}' . | head -50`
  when "bash" then `#{input["cmd"].as_s}`
  else "unknown"
  end
rescue e
  "error: #{e.message}"
end

SCHEMA = JSON.parse(%([{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},
{"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},
{"name":"edit","description":"Edit","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"}},"required":["path","old","new"]}},
{"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
{"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
{"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}]))

def ask(messages)
  uri = URI.parse(API_URL)
  client = HTTP::Client.new(uri.host.not_nil!, port: uri.port, tls: uri.scheme == "https")
  client.before_request { |r| r.headers["Content-Type"] = "application/json"; r.headers["anthropic-version"] = "2023-06-01"; r.headers["x-api-key"] = KEY }
  body = {"model" => MODEL, "max_tokens" => 4096, "system" => "Concise assistant", "messages" => messages, "tools" => SCHEMA}.to_json
  JSON.parse(client.post(uri.path.not_nil!, body: body).body)
end

puts "#{B}nanocode#{R} | #{D}#{MODEL}#{R}\n"
messages = [] of JSON::Any

loop do
  print "#{B}#{BL}❯#{R} "
  input = gets.try(&.strip) || break
  break if input.empty? || input == "/q"
  (messages = [] of JSON::Any; puts "#{G}⏺ Cleared#{R}"; next) if input == "/c"

  messages << JSON.parse({"role" => "user", "content" => input}.to_json)

  loop do
    resp = ask(messages)
    content = resp["content"].as_a
    results = [] of JSON::Any

    content.each do |block|
      puts "\n#{C}⏺#{R} #{block["text"]}" if block["type"].as_s == "text"
      if block["type"].as_s == "tool_use"
        name = block["name"].as_s
        puts "\n#{G}⏺ #{name}#{R}"
        result = tool(name, block["input"])
        puts "  #{D}⎿ #{result.lines.first? || ""}#{R}"
        results << JSON.parse({"type" => "tool_result", "tool_use_id" => block["id"], "content" => result}.to_json)
      end
    end

    messages << JSON.parse({"role" => "assistant", "content" => content}.to_json)
    break if results.empty?
    messages << JSON.parse({"role" => "user", "content" => results}.to_json)
  end
  puts
end
