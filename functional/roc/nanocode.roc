# nanocode - minimal claude code alternative (Roc)
# roc run nanocode.roc
# Roc is a fast, friendly, functional language (2019+)

app "nanocode"
    packages { pf: "https://github.com/roc-lang/basic-cli/releases/download/0.8.1/roc-basic-cli.tar.br" }
    imports [
        pf.Stdout,
        pf.Stdin,
        pf.Task.{ Task },
        pf.Http,
        pf.Env,
    ]
    provides [main] to pf

# ANSI colors
r = "\u(001b)[0m"
b = "\u(001b)[1m"
d = "\u(001b)[2m"
c = "\u(001b)[36m"
g = "\u(001b)[32m"
bl = "\u(001b)[34m"

# Tool execution
tool : Str, { path ? Str, content ? Str, cmd ? Str, pat ? Str } -> Task Str *
tool = \name, input ->
    when name is
        "read" ->
            path = input.path |> Result.withDefault ""
            # File reading would happen here
            Task.ok "1| file content"
        
        "write" ->
            path = input.path |> Result.withDefault ""
            content = input.content |> Result.withDefault ""
            # File writing would happen here
            Task.ok "ok"
        
        "bash" ->
            cmd = input.cmd |> Result.withDefault ""
            # Command execution would happen here
            Task.ok "output"
        
        _ ->
            Task.ok "unknown"

# Schema definition
schema =
    """
    [{"name":"read","description":"Read file","input_schema":{"type":"object","properties":{"path":{"type":"string"}},"required":["path"]}},
    {"name":"write","description":"Write file","input_schema":{"type":"object","properties":{"path":{"type":"string"},"content":{"type":"string"}},"required":["path","content"]}},
    {"name":"bash","description":"Run command","input_schema":{"type":"object","properties":{"cmd":{"type":"string"}},"required":["cmd"]}}]
    """

# API call
ask : Str, Str, List { role : Str, content : Str } -> Task Str *
ask = \key, model, messages ->
    body = 
        """
        {"model":"$(model)","max_tokens":4096,"system":"Concise assistant","messages":$(Inspect.toStr messages),"tools":$(schema)}
        """
    
    Http.send {
        method: Post,
        headers: [
            { name: "Content-Type", value: "application/json" },
            { name: "anthropic-version", value: "2023-06-01" },
            { name: "x-api-key", value: key },
        ],
        url: "https://api.anthropic.com/v1/messages",
        body: Http.stringBody "application/json" body,
        timeout: NoTimeout,
    }
    |> Task.map .body
    |> Task.mapErr \_ -> "API error"

# Main loop
loop : Str, Str, List { role : Str, content : Str } -> Task {} *
loop = \key, model, messages ->
    _ <- Stdout.write "$(b)$(bl)❯$(r) " |> Task.await
    input <- Stdin.line |> Task.await
    
    when Str.trim input is
        "" ->
            loop key model messages
        
        "/q" ->
            Task.ok {}
        
        "/c" ->
            _ <- Stdout.line "$(g)⏺ Cleared$(r)" |> Task.await
            loop key model []
        
        userInput ->
            newMessages = List.append messages { role: "user", content: userInput }
            
            # Would call API and process tools here
            _ <- Stdout.line "\n$(c)⏺$(r) I'm your Roc-powered assistant!" |> Task.await
            _ <- Stdout.line "" |> Task.await
            
            loop key model newMessages

# Entry point
main : Task {} *
main =
    key <- Env.var "ANTHROPIC_API_KEY" |> Task.await
    model <- Env.var "MODEL" |> Task.map (Result.withDefault "claude-sonnet-4-20250514") |> Task.await
    
    _ <- Stdout.line "$(b)nanocode$(r) | $(d)Roc + $(model)$(r)\n" |> Task.await
    
    loop key model []
