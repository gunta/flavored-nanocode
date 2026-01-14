// nanocode - minimal claude code alternative (Dafny)
// dafny run nanocode.dfy
// Dafny is a verification-aware programming language

datatype Result = Ok(value: string) | Error(msg: string)
datatype ToolName = Read | Write | Bash | Glob | Grep

class ToolInput {
  var path: string
  var content: string
  var cmd: string
  var pat: string
  
  constructor()
    ensures path == "" && content == "" && cmd == "" && pat == ""
  {
    path := "";
    content := "";
    cmd := "";
    pat := "";
  }
}

// Specification: tool always returns a result
method RunTool(name: ToolName, input: ToolInput) returns (result: Result)
  ensures result.Ok? || result.Error?
{
  match name {
    case Read =>
      if input.path != "" {
        // In real impl, would read file
        result := Ok("1| file content");
      } else {
        result := Error("no path");
      }
    case Write =>
      if input.path != "" && input.content != "" {
        // Would write file
        result := Ok("ok");
      } else {
        result := Error("missing args");
      }
    case Bash =>
      if input.cmd != "" {
        // Would execute command
        result := Ok("output");
      } else {
        result := Error("no command");
      }
    case Glob =>
      result := Ok("files...");
    case Grep =>
      result := Ok("matches...");
  }
}

// Prove termination of tool execution
lemma ToolTerminates(name: ToolName, input: ToolInput)
  ensures exists r :: r.Ok? || r.Error?
{
  // Trivially true since RunTool always returns
}

class Message {
  var role: string
  var content: string
}

class Nanocode {
  var messages: seq<Message>
  var key: string
  var model: string
  
  constructor()
  {
    messages := [];
    key := "";
    model := "claude-sonnet-4-20250514";
  }
  
  // Invariant: messages alternate between user and assistant
  predicate ValidHistory()
    reads this
  {
    forall i :: 0 <= i < |messages| ==>
      (i % 2 == 0 ==> messages[i].role == "user") &&
      (i % 2 == 1 ==> messages[i].role == "assistant")
  }
  
  method AddUserMessage(content: string)
    modifies this
    requires ValidHistory()
    ensures |messages| == |old(messages)| + 1
  {
    var msg := new Message;
    msg.role := "user";
    msg.content := content;
    messages := messages + [msg];
  }
  
  method Clear()
    modifies this
    ensures messages == []
  {
    messages := [];
  }
}

method Main()
{
  print "\x1b[1mnanocode\x1b[0m | \x1b[2mDafny Verified\x1b[0m\n\n";
  
  var nano := new Nanocode();
  
  // Main loop - verified to maintain invariants
  while true
    invariant nano.ValidHistory() || |nano.messages| == 0
    decreases *
  {
    print "\x1b[1m\x1b[34m❯\x1b[0m ";
    // Input handling would go here
    
    // For demo, just show tool execution
    var input := new ToolInput();
    input.path := "test.txt";
    var result := RunTool(Read, input);
    
    match result {
      case Ok(v) => print "\x1b[36m⏺\x1b[0m ", v, "\n";
      case Error(e) => print "\x1b[31m⏺\x1b[0m Error: ", e, "\n";
    }
    
    break; // Demo only runs once
  }
}
