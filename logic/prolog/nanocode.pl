% nanocode - minimal claude code alternative (Prolog)
% swipl -s nanocode.pl -g main
% Prolog is the OG logic programming language - AI before AI was cool!

:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(readutil)).
:- use_module(library(process)).

% ANSI colors
r("\x1b[0m").
b("\x1b[1m").
d("\x1b[2m").
c("\x1b[36m").
g("\x1b[32m").
bl("\x1b[34m").

% Tool definitions - as logical facts!
tool_schema(read, "Read file", [path, offset?, limit?]).
tool_schema(write, "Write file", [path, content]).
tool_schema(edit, "Replace text", [path, old, new, all?]).
tool_schema(glob, "Find files", [pat, path?]).
tool_schema(grep, "Search content", [pat, path?]).
tool_schema(bash, "Run command", [cmd]).

% Tool execution - pattern matching is natural in Prolog
execute_tool(read, Input, Result) :-
    member(path-Path, Input),
    (member(offset-Off, Input) -> true ; Off = 0),
    (member(limit-Lim, Input) -> true ; Lim = 1_000_000),
    read_file_to_string(Path, Content, []),
    split_string(Content, "\n", "", Lines0),
    drop_n(Lines0, Off, AfterOff),
    take_n(AfterOff, Lim, Take),
    number_lines(Take, Off+1, Numbered),
    atomics_to_string(Numbered, "\n", Result), !.

execute_tool(write, Input, "ok") :-
    member(path-Path, Input),
    member(content-Content, Input),
    setup_call_cleanup(open(Path, write, Stream), write(Stream, Content), close(Stream)).

execute_tool(edit, Input, Result) :-
    member(path-Path, Input),
    member(old-Old, Input),
    member(new-New, Input),
    (member(all-true, Input) -> All=true ; All=false),
    read_file_to_string(Path, Text, []),
    findall(_, sub_string(Text, _, _, _, Old), Matches),
    length(Matches, Count),
    ( Count =:= 0 -> Result = "error: old_string not found"
    ; Count > 1, All = false -> format(atom(Result), "error: old_string appears ~d times, use all=true", [Count])
    ; ( All -> replace_all(Text, Old, New, Updated) ; replace_first(Text, Old, New, Updated) ),
      setup_call_cleanup(open(Path, write, S), write(S, Updated), close(S)),
      Result = "ok"
    ).

execute_tool(glob, Input, Result) :-
    (member(path-Base, Input) -> true ; Base = "."),
    member(pat-Pat, Input),
    atomic_list_concat(["find ", Base, " -name '", Pat, "' -maxdepth 10 2>/dev/null"], Cmd),
    run_cmd(Cmd, Result).

execute_tool(grep, Input, Result) :-
    (member(path-Base, Input) -> true ; Base = "."),
    member(pat-Pat, Input),
    atomic_list_concat(["grep -R '", Pat, "' ", Base, " -n 2>/dev/null | head -50"], Cmd),
    run_cmd(Cmd, Str),
    (Str = "" -> Result = "none" ; Result = Str).

execute_tool(bash, Input, Result) :-
    member(cmd-Cmd, Input),
    run_cmd(Cmd, Result).

execute_tool(_, _, "unknown tool").

% Number lines helper
number_lines([], _, []).
number_lines([H|T], N, [Line|Rest]) :-
    format(atom(Line), "~d| ~s", [N, H]),
    N1 is N + 1,
    number_lines(T, N1, Rest).

replace_first(Text, Old, New, Out) :-
    ( sub_atom(Text, Before, _, After, Old) ->
        sub_atom(Text, 0, Before, _, Prefix),
        sub_atom(Text, _, After, 0, Suffix),
        atom_concat(Prefix, New, Temp),
        atom_concat(Temp, Suffix, Out)
    ; Out = Text).

replace_all(Text, Old, New, Out) :-
    atomic_list_concat(Parts, Old, Text),
    atomic_list_concat(Parts, New, Out).

run_cmd(Cmd, Result) :-
    process_create(path(sh), ['-c', Cmd], [stdout(pipe(Out))]),
    read_string(Out, _, Result),
    close(Out).

drop_n(L, 0, L) :- !.
drop_n([], _, []).
drop_n([_|T], N, R) :- N > 0, N1 is N - 1, drop_n(T, N1, R).

take_n(_, 0, []) :- !.
take_n([], _, []).
take_n([H|T], N, [H|R]) :- N > 0, N1 is N - 1, take_n(T, N1, R).

% Message handling - Prolog's natural domain
message(Role, Content, json([role=Role, content=Content])).

api_url(URL) :-
    (   getenv('API_URL', U) -> URL = U
    ;   getenv('OPENROUTER_API_KEY', _) -> URL = "https://openrouter.ai/api/v1/messages"
    ;   URL = "https://api.anthropic.com/v1/messages"
    ).

auth_header(HeaderName, Value) :-
    (   getenv('OPENROUTER_API_KEY', K) -> HeaderName = 'Authorization', format(atom(Value), "Bearer ~w", [K])
    ;   getenv('ANTHROPIC_API_KEY', K) -> HeaderName = 'x-api-key', Value = K
    ).

tools_schema([
    json([name="read",description="Read file",input_schema=json([type="object",properties=json([path=json([type="string"]),offset=json([type="integer"]),limit=json([type="integer"])]),required=["path"]])]),
    json([name="write",description="Write file",input_schema=json([type="object",properties=json([path=json([type="string"]),content=json([type="string"])]),required=["path","content"]])]),
    json([name="edit",description="Replace text",input_schema=json([type="object",properties=json([path=json([type="string"]),old=json([type="string"]),new=json([type="string"]),all=json([type="boolean"])]),required=["path","old","new"]])]),
    json([name="glob",description="Find files",input_schema=json([type="object",properties=json([pat=json([type="string"]),path=json([type="string"])]),required=["pat"]])]),
    json([name="grep",description="Search files",input_schema=json([type="object",properties=json([pat=json([type="string"]),path=json([type="string"])]),required=["pat"]])]),
    json([name="bash",description="Run command",input_schema=json([type="object",properties=json([cmd=json([type="string"])]),required=["cmd"]])])
]).

ask_claude(Messages, Content) :-
    getenv('MODEL', Model0) -> Model=Model0 ; (getenv('OPENROUTER_API_KEY', _) -> Model="anthropic/claude-opus-4-5" ; Model="claude-opus-4-5"),
    tools_schema(Tools),
    api_url(URL),
    auth_header(HName, HVal),
    http_post(URL,
        json(json([
            model=Model,
            max_tokens=8192,
            system="Concise coding assistant",
            messages=Messages,
            tools=Tools
        ])),
        Content,
        [ json_object(dict),
          request_header('Content-Type'='application/json'),
          request_header('anthropic-version'='2023-06-01'),
          request_header(HName=HVal)
        ]).

agent_loop(Messages, Final) :-
    ask_claude(Messages, Response),
    get_dict(content, Response, ContentBlocks),
    print_blocks(ContentBlocks, Results),
    append(Messages, [json([role=assistant, content=ContentBlocks])], Messages1),
    (   Results = [] -> Final = Messages1
    ;   append(Messages1, [json([role=user, content=Results])], Messages2),
        agent_loop(Messages2, Final)
    ).

print_blocks([], []).
print_blocks([Block|Rest], Results) :-
    (   get_dict(type, Block, "text") ->
        c(C), r(R),
        get_dict(text, Block, Txt),
        format("\n~s⏺~s ~s", [C, R, Txt]),
        print_blocks(Rest, Results)
    ;   get_dict(type, Block, "tool_use") ->
        g(G), r(R),
        get_dict(name, Block, Name),
        format("\n~s⏺ ~w~s\n", [G, Name, R]),
        get_dict(input, Block, Input),
        convert_input(Input, Pairs),
        execute_tool(Name, Pairs, ResAtom),
        d(D),
        sub_atom(ResAtom, 0, _, _, Preview),
        format("  ~s⎿ ~s~s\n", [D, Preview, R]),
        print_blocks(Rest, More),
        get_dict(id, Block, Id),
        Results = [json([type="tool_result",tool_use_id=Id,content=ResAtom])|More]
    ;   print_blocks(Rest, Results)
    ).

convert_input(Dict, Pairs) :-
    dict_pairs(Dict, _, P),
    maplist([K-V,KSym-V]>>(atom_string(KSym,K)), P, Pairs).

% Main REPL loop - using Prolog's backtracking!
repl(Messages) :-
    b(B), bl(BL), r(R), g(G),
    format("~s~s❯~s ", [B, BL, R]),
    read_line_to_string(user_input, Input),
    (   Input = "" -> repl(Messages)
    ;   Input = "/q" -> true
    ;   Input = "/c" -> format("~s⏺ Cleared~s~n", [G, R]), repl([])
    ;   message(user, Input, Msg),
        append(Messages, [Msg], NewMessages),
        agent_loop(NewMessages, FinalMessages),
        format("~n", []),
        repl(FinalMessages)
    ).

% Entry point
main :-
    b(B), r(R), d(D),
    format("~snanocode~s | ~sProlog - Logic Programming~s~n~n", [B, R, D, R]),
    repl([]).

% In Prolog, the AI agent is a logical inference engine
% Tool selection is just pattern matching
% The agentic loop is just backtracking search
% We've come full circle!
