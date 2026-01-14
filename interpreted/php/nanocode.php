#!/usr/bin/env php
<?php
// nanocode - minimal claude code alternative (PHP)
$KEY = getenv('ANTHROPIC_API_KEY');
$MODEL = getenv('MODEL') ?: 'claude-sonnet-4-20250514';
[$R, $B, $D, $C, $G, $BL] = ["\e[0m", "\e[1m", "\e[2m", "\e[36m", "\e[32m", "\e[34m"];

$tools = [
    'read' => fn($a) => implode("\n", array_map(fn($l, $i) => ($i+1)."| $l", file($a['path']) ?: [], array_keys(file($a['path']) ?: []))),
    'write' => fn($a) => file_put_contents($a['path'], $a['content']) !== false ? 'ok' : 'error',
    'edit' => fn($a) => (($t = @file_get_contents($a['path'])) && strpos($t, $a['old']) !== false) 
        ? (file_put_contents($a['path'], preg_replace('/'.preg_quote($a['old'], '/').'/', $a['new'], $t, 1)) ? 'ok' : 'error') 
        : 'error: not found',
    'glob' => fn($a) => implode("\n", array_slice(glob("**/{$a['pat']}", GLOB_BRACE) ?: ['none'], 0, 50)),
    'grep' => fn($a) => implode("\n", array_slice(array_filter(explode("\n", shell_exec("grep -rn '{$a['pat']}' . 2>/dev/null") ?: '')), 0, 50)) ?: 'none',
    'bash' => fn($a) => shell_exec($a['cmd'] . ' 2>&1') ?: '',
];

$schema = [
    ['name' => 'read', 'description' => 'Read file', 'input_schema' => ['type' => 'object', 'properties' => ['path' => ['type' => 'string']], 'required' => ['path']]],
    ['name' => 'write', 'description' => 'Write file', 'input_schema' => ['type' => 'object', 'properties' => ['path' => ['type' => 'string'], 'content' => ['type' => 'string']], 'required' => ['path', 'content']]],
    ['name' => 'edit', 'description' => 'Edit file', 'input_schema' => ['type' => 'object', 'properties' => ['path' => ['type' => 'string'], 'old' => ['type' => 'string'], 'new' => ['type' => 'string']], 'required' => ['path', 'old', 'new']]],
    ['name' => 'glob', 'description' => 'Find files', 'input_schema' => ['type' => 'object', 'properties' => ['pat' => ['type' => 'string']], 'required' => ['pat']]],
    ['name' => 'grep', 'description' => 'Search', 'input_schema' => ['type' => 'object', 'properties' => ['pat' => ['type' => 'string']], 'required' => ['pat']]],
    ['name' => 'bash', 'description' => 'Run command', 'input_schema' => ['type' => 'object', 'properties' => ['cmd' => ['type' => 'string']], 'required' => ['cmd']]],
];

function ask($messages, $KEY, $MODEL, $schema) {
    $ch = curl_init('https://api.anthropic.com/v1/messages');
    curl_setopt_array($ch, [
        CURLOPT_RETURNTRANSFER => true, CURLOPT_POST => true,
        CURLOPT_HTTPHEADER => ['Content-Type: application/json', 'anthropic-version: 2023-06-01', "x-api-key: $KEY"],
        CURLOPT_POSTFIELDS => json_encode(['model' => $MODEL, 'max_tokens' => 4096, 'system' => 'Concise coding assistant', 'messages' => $messages, 'tools' => $schema])
    ]);
    $resp = json_decode(curl_exec($ch), true);
    curl_close($ch);
    return $resp;
}

echo "{$B}nanocode{$R} | {$D}{$MODEL}{$R}\n\n";
$messages = [];

while (true) {
    echo "{$B}{$BL}❯{$R} ";
    $input = trim(fgets(STDIN));
    if ($input === false || $input === '/q') break;
    if ($input === '') continue;
    if ($input === '/c') { $messages = []; echo "{$G}⏺ Cleared{$R}\n"; continue; }

    $messages[] = ['role' => 'user', 'content' => $input];

    while (true) {
        $resp = ask($messages, $KEY, $MODEL, $schema);
        $content = $resp['content'] ?? [];
        $results = [];

        foreach ($content as $block) {
            if ($block['type'] === 'text') echo "\n{$C}⏺{$R} {$block['text']}";
            if ($block['type'] === 'tool_use') {
                $name = $block['name'];
                echo "\n{$G}⏺ {$name}{$R}\n";
                $result = $tools[$name]($block['input']);
                echo "  {$D}⎿ " . explode("\n", $result)[0] . "{$R}\n";
                $results[] = ['type' => 'tool_result', 'tool_use_id' => $block['id'], 'content' => $result];
            }
        }

        $messages[] = ['role' => 'assistant', 'content' => $content];
        if (empty($results)) break;
        $messages[] = ['role' => 'user', 'content' => $results];
    }
    echo "\n";
}
