#!/usr/bin/env ruby
# nanocode - minimal claude code alternative (Ruby)
require 'net/http'
require 'json'

KEY = ENV['ANTHROPIC_API_KEY']
MODEL = ENV['MODEL'] || 'claude-sonnet-4-20250514'
API_URL = ENV['API_URL'] || 'https://api.anthropic.com/v1/messages'
R, B, D, C, G, BL = "\e[0m", "\e[1m", "\e[2m", "\e[36m", "\e[32m", "\e[34m"

TOOLS = {
  read: ->(a) { File.readlines(a['path']).each_with_index.map { |l, i| "#{i + 1}| #{l}" }.join rescue "error: #{$!}" },
  write: ->(a) { File.write(a['path'], a['content']); 'ok' rescue "error: #{$!}" },
  edit: ->(a) { t = File.read(a['path']); t.include?(a['old']) ? (File.write(a['path'], t.sub(a['old'], a['new'])); 'ok') : 'error: not found' rescue "error: #{$!}" },
  glob: ->(a) { Dir.glob("**/#{a['pat']}").first(50).join("\n").then { |r| r.empty? ? 'none' : r } },
  grep: ->(a) { Dir.glob('**/*').select { |f| File.file?(f) }.flat_map { |f| File.readlines(f).each_with_index.select { |l, _| l =~ /#{a['pat']}/ }.map { |l, i| "#{f}:#{i + 1}:#{l.chomp}" } rescue [] }.first(50).join("\n").then { |r| r.empty? ? 'none' : r } },
  bash: ->(a) { `#{a['cmd']} 2>&1`.lines.first(100).join rescue "error: #{$!}" }
}

SCHEMA = [
  { name: 'read', description: 'Read file', input_schema: { type: 'object', properties: { path: { type: 'string' } }, required: ['path'] } },
  { name: 'write', description: 'Write file', input_schema: { type: 'object', properties: { path: { type: 'string' }, content: { type: 'string' } }, required: %w[path content] } },
  { name: 'edit', description: 'Edit file', input_schema: { type: 'object', properties: { path: { type: 'string' }, old: { type: 'string' }, new: { type: 'string' } }, required: %w[path old new] } },
  { name: 'glob', description: 'Find files', input_schema: { type: 'object', properties: { pat: { type: 'string' } }, required: ['pat'] } },
  { name: 'grep', description: 'Search', input_schema: { type: 'object', properties: { pat: { type: 'string' } }, required: ['pat'] } },
  { name: 'bash', description: 'Run command', input_schema: { type: 'object', properties: { cmd: { type: 'string' } }, required: ['cmd'] } }
]

def ask(messages)
  uri = URI(API_URL)
  http = Net::HTTP.new(uri.host, uri.port).tap { |h| h.use_ssl = uri.scheme == 'https' }
  req = Net::HTTP::Post.new(uri, 'Content-Type' => 'application/json', 'anthropic-version' => '2023-06-01', 'x-api-key' => KEY)
  req.body = { model: MODEL, max_tokens: 4096, system: 'Concise coding assistant', messages: messages, tools: SCHEMA }.to_json
  JSON.parse(http.request(req).body)
end

puts "#{B}nanocode#{R} | #{D}#{MODEL}#{R}\n\n"
messages = []

loop do
  print "#{B}#{BL}❯#{R} "
  input = gets&.chomp
  break if input.nil? || input == '/q' || input.empty?
  (messages = []; puts "#{G}⏺ Cleared#{R}"; next) if input == '/c'

  messages << { role: 'user', content: input }

  loop do
    resp = ask(messages)
    content = resp['content'] || []
    results = []

    content.each do |block|
      puts "\n#{C}⏺#{R} #{block['text']}" if block['type'] == 'text'
      if block['type'] == 'tool_use'
        name = block['name']
        puts "\n#{G}⏺ #{name}#{R}(#{D}...#{R})"
        result = TOOLS[name.to_sym]&.call(block['input']) || 'unknown'
        puts "  #{D}⎿ #{result.lines.first&.chomp}#{R}"
        results << { type: 'tool_result', tool_use_id: block['id'], content: result }
      end
    end

    messages << { role: 'assistant', content: content }
    break if results.empty?
    messages << { role: 'user', content: results }
  end
  puts
end
