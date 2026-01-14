---------------------------- MODULE nanocode ----------------------------
(* nanocode - minimal claude code alternative (TLA+) *)
(* TLA+ is for specifying and verifying concurrent systems *)
(* tlc nanocode.tla *)

EXTENDS Sequences, Integers, TLC

CONSTANTS 
    API_KEY,
    MODEL

VARIABLES
    messages,
    state,
    tool_results

(* Tool names *)
ToolNames == {"read", "write", "bash", "glob", "grep"}

(* Message structure *)
Message == [role: {"user", "assistant"}, content: STRING]

(* Tool input structure *)
ToolInput == [
    path: STRING \union {<<>>},
    content: STRING \union {<<>>},
    cmd: STRING \union {<<>>},
    pat: STRING \union {<<>>}
]

(* Tool result *)
ToolResult == [type: {"tool_result"}, tool_use_id: STRING, content: STRING]

(* Initial state *)
Init ==
    /\ messages = <<>>
    /\ state = "waiting"
    /\ tool_results = <<>>

(* User sends a message *)
UserMessage(content) ==
    /\ state = "waiting"
    /\ messages' = Append(messages, [role |-> "user", content |-> content])
    /\ state' = "processing"
    /\ UNCHANGED tool_results

(* Assistant responds with text *)
AssistantText(text) ==
    /\ state = "processing"
    /\ messages' = Append(messages, [role |-> "assistant", content |-> text])
    /\ state' = "waiting"
    /\ UNCHANGED tool_results

(* Assistant requests tool use *)
AssistantToolUse(tool_name, tool_id, input) ==
    /\ state = "processing"
    /\ tool_name \in ToolNames
    /\ state' = "tool_execution"
    /\ UNCHANGED <<messages, tool_results>>

(* Tool execution completes *)
ToolComplete(tool_id, result) ==
    /\ state = "tool_execution"
    /\ tool_results' = Append(tool_results, [
        type |-> "tool_result",
        tool_use_id |-> tool_id,
        content |-> result
    ])
    /\ state' = "processing"
    /\ UNCHANGED messages

(* Clear conversation *)
ClearConversation ==
    /\ messages' = <<>>
    /\ tool_results' = <<>>
    /\ state' = "waiting"

(* Quit *)
Quit ==
    /\ state' = "done"
    /\ UNCHANGED <<messages, tool_results>>

(* Next state relation *)
Next ==
    \/ \E content \in STRING: UserMessage(content)
    \/ \E text \in STRING: AssistantText(text)
    \/ \E name \in ToolNames, id \in STRING, input \in ToolInput:
        AssistantToolUse(name, id, input)
    \/ \E id, result \in STRING: ToolComplete(id, result)
    \/ ClearConversation
    \/ Quit

(* Type invariant *)
TypeInvariant ==
    /\ \A i \in 1..Len(messages): messages[i] \in Message
    /\ state \in {"waiting", "processing", "tool_execution", "done"}

(* Safety: messages alternate between user and assistant *)
MessageAlternation ==
    \A i \in 1..(Len(messages) - 1):
        messages[i].role /= messages[i+1].role

(* Liveness: system eventually responds *)
EventuallyResponds ==
    state = "processing" ~> state = "waiting"

(* Specification *)
Spec ==
    /\ Init
    /\ [][Next]_<<messages, state, tool_results>>
    /\ WF_<<messages, state, tool_results>>(Next)

(* Properties to check *)
THEOREM Spec => []TypeInvariant
THEOREM Spec => []MessageAlternation
THEOREM Spec => EventuallyResponds

=============================================================================
