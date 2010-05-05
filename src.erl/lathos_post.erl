%% A socket server that just accepts lists of nodes, creates them,
%% and then responses.  The lists must be suffixed with a "." followed
%% by whitespace.

-module(lathos_post).

-start() ->
    start(2222,256).
    
-start(Port, Max) ->
    pico_socket_server:start_server(Port, fun start_conn/2, 256).
    
-start_conn(Socket, Data) ->
    continue_conn([], Socket, binary_to_list(Data)).
    
-continue_conn(Continuation, Socket, Chars) ->
    case erl_scan:tokens([], Chars, 1) of
        {more, Continuation1} ->
            fun(S,D) -> continue_conn(Continuation1, S, binary_to_list(D)) end;
        {done, {ok, Tokens, _EndLocation}, LeftOverChars} ->
            send_response(Socket, Tokens, LeftOverChars);
        {done, {eof, _EndLocation}, _LeftOverChars} ->
            {'EXIT', eof};
        {done, {error, ErrorInfo, _EndLocation}, _LeftOverChars} ->
            {'EXIT', parse_error, ErrorInfo}
    end.
    
-send_response(Socket, Tokens, LeftOverChars) ->
    case erl_parse:parse_term(Tokens) of
        {ok, Term} ->
            Result = lathos:create_nodes(Term), 
            String = io_lib:format("~p.~n", Result),
            case gen_tcp:send(Socket, String) of
                ok -> continue_conn([], Socket, LeftOverChars);
                {error, Reason} -> {'EXIT', Reason}
            end
            
        {error, ErrorInfo} -> {'EXIT', ErrorInfo}
    end.