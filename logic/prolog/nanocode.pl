% nanocode - minimal claude code alternative (Prolog)
% swipl -s nanocode.pl -g main
% Prolog is the OG logic programming language - AI before AI was cool!

:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(readutil)).

% ANSI colors
r("\x1b[0m").
b("\x1b[1m").
d("\x1b[2m").
c("\x1b[36m").
g("\x1b[32m").
bl("\x1b[34m").

% Tool definitions - as logical facts!
tool_schema(read, "Read file", [path]).
tool_schema(write, "Write file", [path, content]).
tool_schema(bash, "Run command", [cmd]).
tool_schema(glob, "Find files", [pat]).
tool_schema(grep, "Search content", [pat]).

% Tool execution - pattern matching is natural in Prolog
execute_tool(read, Input, Result) :-
    member(path-Path, Input),
    read_file_to_string(Path, Content, []),
    split_string(Content, "\n", "", Lines),
    number_lines(Lines, 1, Numbered),
    atomics_to_string(Numbered, "\n", Result).

execute_tool(write, Input, "ok") :-
    member(path-Path, Input),
    member(content-Content, Input),
    open(Path, write, Stream),
    write(Stream, Content),
    close(Stream).

execute_tool(bash, Input, Result) :-
    member(cmd-Cmd, Input),
    shell(Cmd, Result).

execute_tool(_, _, "unknown tool").

% Number lines helper
number_lines([], _, []).
number_lines([H|T], N, [Line|Rest]) :-
    format(atom(Line), "~d| ~s", [N, H]),
    N1 is N + 1,
    number_lines(T, N1, Rest).

% Message handling - Prolog's natural domain
message(user, Content, json([role=user, content=Content])).
message(assistant, Content, json([role=assistant, content=Content])).

% API call (would need actual HTTP implementation)
ask_claude(Messages, _Response) :-
    % HTTP request would go here
    format("~n~s⏺~s Prolog speaks logic!~n", ["\x1b[36m", "\x1b[0m"]).

% Main REPL loop - using Prolog's backtracking!
repl(Messages) :-
    b(B), bl(BL), r(R), g(G),
    format("~s~s❯~s ", [B, BL, R]),
    read_line_to_string(user_input, Input),
    (   Input = ""
    ->  repl(Messages)
    ;   Input = "/q"
    ->  true
    ;   Input = "/c"
    ->  format("~s⏺ Cleared~s~n", [G, R]),
        repl([])
    ;   message(user, Input, Msg),
        append(Messages, [Msg], NewMessages),
        ask_claude(NewMessages, _),
        format("~n", []),
        repl(NewMessages)
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
