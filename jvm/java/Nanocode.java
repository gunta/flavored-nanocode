///usr/bin/env jbang "$0" "$@" ; exit $?
//JAVA 17+
// nanocode - minimal claude code alternative (Java)
import java.io.*; import java.net.*; import java.net.http.*; import java.nio.file.*;
import java.util.*; import java.util.regex.*;
import java.util.stream.*;

public class Nanocode {
    static final String R="\033[0m", B="\033[1m", D="\033[2m", C="\033[36m", G="\033[32m", BL="\033[34m";
    static final String KEY = System.getenv("ANTHROPIC_API_KEY");
    static final String MODEL = System.getenv().getOrDefault("MODEL", "claude-sonnet-4-20250514");
    static final HttpClient client = HttpClient.newHttpClient();
    
    static String runTool(String name, Map<String,Object> input) {
        try { switch(name) {
            case "read": return Files.readAllLines(Path.of((String)input.get("path"))).stream()
                .map(l -> (Files.readAllLines(Path.of((String)input.get("path"))).indexOf(l)+1) + "| " + l)
                .collect(Collectors.joining("\n"));
            case "write": Files.writeString(Path.of((String)input.get("path")), (String)input.get("content")); return "ok";
            case "edit": var p = Path.of((String)input.get("path")); var t = Files.readString(p);
                if(!t.contains((String)input.get("old"))) return "error: not found";
                Files.writeString(p, t.replace((String)input.get("old"), (String)input.get("new"))); return "ok";
            case "glob": return new ProcessBuilder("sh","-c","find . -name '"+input.get("pat")+"' | head -50").start()
                .inputReader().lines().collect(Collectors.joining("\n"));
            case "grep": return new ProcessBuilder("sh","-c","grep -rn '"+input.get("pat")+"' . | head -50").start()
                .inputReader().lines().collect(Collectors.joining("\n"));
            case "bash": var proc = new ProcessBuilder("sh","-c",(String)input.get("cmd")).redirectErrorStream(true).start();
                return proc.inputReader().lines().collect(Collectors.joining("\n"));
        }} catch(Exception e) { return "error: " + e.getMessage(); }
        return "unknown";
    }
    
    static String ask(List<Map<String,Object>> messages) throws Exception {
        var schema = """
        [{"name":"read","description":"Read","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},
        {"name":"write","description":"Write","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},
        {"name":"edit","description":"Edit","input_schema":{"type":"object","properties":{"path":{"type":"string"},"old":{"type":"string"},"new":{"type":"string"}},"required":["path","old","new"]}},
        {"name":"glob","description":"Find","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
        {"name":"grep","description":"Search","input_schema":{"type":"object","properties":{"pat":{"type":"string"}},"required":["pat"]}},
        {"name":"bash","description":"Run","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}]""";
        var body = String.format("{\"model\":\"%s\",\"max_tokens\":4096,\"system\":\"Concise assistant\",\"messages\":%s,\"tools\":%s}",
            MODEL, toJson(messages), schema);
        var req = HttpRequest.newBuilder(URI.create("https://api.anthropic.com/v1/messages"))
            .header("Content-Type","application/json").header("anthropic-version","2023-06-01").header("x-api-key",KEY)
            .POST(HttpRequest.BodyPublishers.ofString(body)).build();
        return client.send(req, HttpResponse.BodyHandlers.ofString()).body();
    }
    
    @SuppressWarnings("unchecked")
    static String toJson(Object o) {
        if(o instanceof String s) return "\"" + s.replace("\\","\\\\").replace("\"","\\\"").replace("\n","\\n") + "\"";
        if(o instanceof Number n) return n.toString();
        if(o instanceof Boolean b) return b.toString();
        if(o instanceof List l) return "[" + l.stream().map(Nanocode::toJson).collect(Collectors.joining(",")) + "]";
        if(o instanceof Map m) return "{" + ((Map<String,Object>)m).entrySet().stream()
            .map(e -> "\""+e.getKey()+"\":"+toJson(e.getValue())).collect(Collectors.joining(",")) + "}";
        return "null";
    }
    
    @SuppressWarnings("unchecked")
    public static void main(String[] args) throws Exception {
        System.out.println(B+"nanocode"+R+" | "+D+MODEL+R+"\n");
        var messages = new ArrayList<Map<String,Object>>();
        var scanner = new Scanner(System.in);
        
        while(true) {
            System.out.print(B+BL+"❯"+R+" ");
            if(!scanner.hasNextLine()) break;
            var input = scanner.nextLine().trim();
            if(input.isEmpty()) continue;
            if(input.equals("/q")) break;
            if(input.equals("/c")) { messages.clear(); System.out.println(G+"⏺ Cleared"+R); continue; }
            
            messages.add(Map.of("role","user","content",input));
            
            while(true) {
                var resp = ask(messages);
                var content = parseArray(resp.substring(resp.indexOf("\"content\":")+10));
                var results = new ArrayList<Map<String,Object>>();
                
                for(var block : (List<Map<String,Object>>)content) {
                    if("text".equals(block.get("type"))) System.out.println("\n"+C+"⏺"+R+" "+block.get("text"));
                    if("tool_use".equals(block.get("type"))) {
                        var name = (String)block.get("name");
                        System.out.println("\n"+G+"⏺ "+name+R);
                        var result = runTool(name, (Map<String,Object>)block.get("input"));
                        System.out.println("  "+D+"⎿ "+result.lines().findFirst().orElse("")+R);
                        results.add(Map.of("type","tool_result","tool_use_id",block.get("id"),"content",result));
                    }
                }
                messages.add(Map.of("role","assistant","content",content));
                if(results.isEmpty()) break;
                messages.add(Map.of("role","user","content",results));
            }
            System.out.println();
        }
    }
    
    static Object parseArray(String s) { // Minimal JSON parser for content array
        var list = new ArrayList<Object>();
        int depth = 0, start = 0; boolean inStr = false;
        for(int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if(c == '"' && (i == 0 || s.charAt(i-1) != '\\')) inStr = !inStr;
            if(!inStr) {
                if(c == '[' || c == '{') { if(depth++ == 0) start = i + 1; }
                if(c == ']' || c == '}') { if(--depth == 0) { parseObj(s.substring(start, i), list); return list; }}
            }
        }
        return list;
    }
    
    static void parseObj(String s, List<Object> list) {
        for(var part : s.split("(?<=\\}),(?=\\{)")) {
            var map = new HashMap<String,Object>();
            var m = Pattern.compile("\"(\\w+)\":(?:\"([^\"]*)\"|([\\d.]+)|(\\{[^}]*\\}))").matcher(part);
            while(m.find()) map.put(m.group(1), m.group(2) != null ? m.group(2) : m.group(3) != null ? m.group(3) : parseInput(m.group(4)));
            if(!map.isEmpty()) list.add(map);
        }
    }
    
    static Map<String,Object> parseInput(String s) {
        var map = new HashMap<String,Object>();
        var m = Pattern.compile("\"(\\w+)\":\"([^\"]*)\"").matcher(s);
        while(m.find()) map.put(m.group(1), m.group(2).replace("\\n","\n").replace("\\\"","\""));
        return map;
    }
}
