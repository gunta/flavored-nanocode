#!/usr/bin/env dart
// nanocode - minimal claude code alternative (Dart)
import 'dart:convert';
import 'dart:io';

final KEY = Platform.environment['ANTHROPIC_API_KEY']!;
final MODEL = Platform.environment['MODEL'] ?? 'claude-sonnet-4-20250514';
const R = '\x1b[0m', B = '\x1b[1m', D = '\x1b[2m', C = '\x1b[36m', G = '\x1b[32m', BL = '\x1b[34m';

String tool(String name, Map input) {
  try {
    switch (name) {
      case 'read': return File(input['path']).readAsLinesSync().asMap().entries.map((e) => '${e.key + 1}| ${e.value}').join('\n');
      case 'write': File(input['path']).writeAsStringSync(input['content']); return 'ok';
      case 'edit': var t = File(input['path']).readAsStringSync(); if (!t.contains(input['old'])) return 'error: not found';
        File(input['path']).writeAsStringSync(t.replaceFirst(input['old'], input['new'])); return 'ok';
      case 'glob': return Process.runSync('find', ['.', '-name', input['pat']]).stdout.toString().split('\n').take(50).join('\n');
      case 'grep': return Process.runSync('grep', ['-rn', input['pat'], '.']).stdout.toString().split('\n').take(50).join('\n');
      case 'bash': return Process.runSync('sh', ['-c', input['cmd']]).stdout.toString();
    }
  } catch (e) { return 'error: $e'; }
  return 'unknown';
}

final schema = jsonDecode('''[{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},
{"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},
{"name":"edit","description":"Edit","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"}},"required":["path","old","new"]}},
{"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
{"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
{"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}]''');

Future<Map> ask(List messages) async {
  final client = HttpClient();
  final req = await client.postUrl(Uri.parse('https://api.anthropic.com/v1/messages'));
  req.headers.set('Content-Type', 'application/json');
  req.headers.set('anthropic-version', '2023-06-01');
  req.headers.set('x-api-key', KEY);
  req.write(jsonEncode({'model': MODEL, 'max_tokens': 4096, 'system': 'Concise assistant', 'messages': messages, 'tools': schema}));
  final resp = await req.close();
  return jsonDecode(await resp.transform(utf8.decoder).join());
}

void main() async {
  print('${B}nanocode$R | $D$MODEL$R\n');
  var messages = <Map>[];

  while (true) {
    stdout.write('$B${BL}❯$R ');
    final input = stdin.readLineSync()?.trim();
    if (input == null || input == '/q') break;
    if (input.isEmpty) continue;
    if (input == '/c') { messages = []; print('${G}⏺ Cleared$R'); continue; }

    messages.add({'role': 'user', 'content': input});

    while (true) {
      final resp = await ask(messages);
      final content = resp['content'] as List;
      final results = <Map>[];

      for (final block in content) {
        if (block['type'] == 'text') print('\n$C⏺$R ${block['text']}');
        if (block['type'] == 'tool_use') {
          final name = block['name'];
          print('\n$G⏺ $name$R');
          final result = tool(name, block['input']);
          print('  $D⎿ ${result.split('\n').first}$R');
          results.add({'type': 'tool_result', 'tool_use_id': block['id'], 'content': result});
        }
      }

      messages.add({'role': 'assistant', 'content': content});
      if (results.isEmpty) break;
      messages.add({'role': 'user', 'content': results});
    }
    print('');
  }
}
