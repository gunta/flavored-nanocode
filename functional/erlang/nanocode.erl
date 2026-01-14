#!/usr/bin/env escript
%% nanocode - minimal claude code alternative (Erlang)
-mode(compile).

main(_) ->
    KEY = os:getenv("ANTHROPIC_API_KEY"),
    MODEL = case os:getenv("MODEL") of false -> "claude-sonnet-4-20250514"; M -> M end,
    io:format("\e[1mnanocode\e[0m | \e[2m~s\e[0m~n~n", [MODEL]),
    inets:start(), ssl:start(),
    loop(KEY, MODEL, []).

loop(KEY, MODEL, Msgs) ->
    io:format("\e[1m\e[34m❯\e[0m "),
    case io:get_line("") of
        eof -> ok;
        "/q\n" -> ok;
        "\n" -> loop(KEY, MODEL, Msgs);
        "/c\n" -> io:format("\e[32m⏺ Cleared\e[0m~n"), loop(KEY, MODEL, []);
        Input ->
            Msg = #{<<"role">> => <<"user">>, <<"content">> => string:trim(Input)},
            NewMsgs = Msgs ++ [Msg],
            {FinalMsgs, _} = agent_loop(KEY, MODEL, NewMsgs),
            io:format("~n"),
            loop(KEY, MODEL, FinalMsgs)
    end.

agent_loop(KEY, MODEL, Msgs) ->
    Resp = ask(KEY, MODEL, Msgs),
    Content = maps:get(<<"content">>, Resp, []),
    {Results, _} = lists:foldl(fun(Block, {Acc, _}) ->
        case maps:get(<<"type">>, Block) of
            <<"text">> -> io:format("~n\e[36m⏺\e[0m ~s", [maps:get(<<"text">>, Block)]), {Acc, ok};
            <<"tool_use">> ->
                Name = maps:get(<<"name">>, Block),
                io:format("~n\e[32m⏺ ~s\e[0m~n", [Name]),
                Result = tool(Name, maps:get(<<"input">>, Block)),
                io:format("  \e[2m⎿ ~s\e[0m~n", [hd(string:split(Result, "\n"))]),
                {Acc ++ [#{<<"type">> => <<"tool_result">>, <<"tool_use_id">> => maps:get(<<"id">>, Block), <<"content">> => list_to_binary(Result)}], ok};
            _ -> {Acc, ok}
        end
    end, {[], ok}, Content),
    NewMsgs = Msgs ++ [#{<<"role">> => <<"assistant">>, <<"content">> => Content}],
    case Results of
        [] -> {NewMsgs, ok};
        _ -> agent_loop(KEY, MODEL, NewMsgs ++ [#{<<"role">> => <<"user">>, <<"content">> => Results}])
    end.

tool(<<"read">>, Input) ->
    Path = binary_to_list(maps:get(<<"path">>, Input)),
    case file:read_file(Path) of
        {ok, Data} -> lists:flatten([io_lib:format("~b| ~s~n", [I, L]) || {I, L} <- lists:zip(lists:seq(1, length(Lines)), Lines = string:split(binary_to_list(Data), "\n", all))]);
        _ -> "error"
    end;
tool(<<"bash">>, Input) ->
    os:cmd(binary_to_list(maps:get(<<"cmd">>, Input)));
tool(<<"glob">>, Input) ->
    os:cmd("find . -name '" ++ binary_to_list(maps:get(<<"pat">>, Input)) ++ "' | head -50");
tool(<<"grep">>, Input) ->
    os:cmd("grep -rn '" ++ binary_to_list(maps:get(<<"pat">>, Input)) ++ "' . | head -50");
tool(_, _) -> "unknown".

ask(KEY, MODEL, Msgs) ->
    Schema = <<"[{\"name\":\"read\",\"description\":\"Read\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"path\":{\"type\":\"string\"}},\"required\":[\"path\"]}},{\"name\":\"bash\",\"description\":\"Run\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"cmd\":{\"type\":\"string\"}},\"required\":[\"cmd\"]}},{\"name\":\"glob\",\"description\":\"Find\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"pat\":{\"type\":\"string\"}},\"required\":[\"pat\"]}},{\"name\":\"grep\",\"description\":\"Search\",\"input_schema\":{\"type\":\"object\",\"properties\":{\"pat\":{\"type\":\"string\"}},\"required\":[\"pat\"]}}]">>,
    Body = jsx:encode(#{<<"model">> => list_to_binary(MODEL), <<"max_tokens">> => 4096, <<"system">> => <<"Concise assistant">>, <<"messages">> => Msgs, <<"tools">> => jsx:decode(Schema)}),
    Headers = [{"Content-Type", "application/json"}, {"anthropic-version", "2023-06-01"}, {"x-api-key", KEY}],
    {ok, {{_, 200, _}, _, RespBody}} = httpc:request(post, {"https://api.anthropic.com/v1/messages", Headers, "application/json", Body}, [], []),
    jsx:decode(list_to_binary(RespBody), [return_maps]).
