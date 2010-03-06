-module(lathos_serve).
-include("lathos.hrl").
-import(pico_utils, [header/1, body/1, str/1]).
-export([start/0, stop/0]).
-export([start_handler/1, event_handler/2, stop_handler/2]).

start()  -> pico_http_server:start(4999, 20, ?MODULE, [1,2,3]).
stop()   -> pico_http_server:stop(4999, foo1234).

start_handler(_) ->
    lathos:start(), 
    lathos:create_node(#id{name="index", version=0}, [], [{text, "Index"}]),
    ok.
    
stop_handler(Reason, ok) ->
    lathos:stop(), 
    {Reason, ok}.
    
colors() -> ["CCCCCC", "BBBBBB", "AAAAAA", "999999"].
color(Depth) -> 
    Colors = colors(),
    lists:nth((Depth rem length(Colors))+1, Colors).

% XXX Expand
escape([$<|T]) -> ["&lt;", escape(T)];
escape([H|T])  -> [H|escape(T)];
escape([])     -> [].

id_to_str(Id) -> lists:flatten(io_lib:format("~p.~p", [Id#id.name, Id#id.version])).
str_to_id(Str) ->
    {match, [_, {Name_s, Name_l}, {Ver_s, Ver_l}]} = re:run(Str, "^(.*)\\.([0-9]+)$"),
    Name = string:substr(Str, Name_s+1, Name_l), 
    {Version, _} = string:to_integer(string:substr(Str, Ver_s+1, Ver_l)),
    #id{name=Name, version=Version}.
    
url(Id) -> 
    IdStr = id_to_str(Id),
    pico_utils:str2urlencoded(IdStr).

link(Text,Id) -> ["<A href='", url(Id), "'>", escape(Text), "</A>"].

html_description([]) ->
    [];
html_description([{text, Text} | D]) ->
    [escape(Text) | html_description(D)];
html_description([{link, Text, Id} | D]) ->
    [link(Text, Id), html_description(D)].

newline() -> 10.
html_tree(Depth, {tree, Node, Children}) ->
    [
        "<DIV",
        " id='", id_to_str(Node#node.id), "'",
        " class='", "'",
        " style='background-color: '", color(Depth), "'",
        ">", newline(),
        "<SPAN class='msg' onclick='toggleId(\"", id_to_str(Node#node.id), "\")'>",
        html_description(Node#node.description),
        "</SPAN>", newline(),
        lists:map(fun(C) -> html_tree(Depth+1, C) end, Children),
        "</DIV>", newline()
    ].
    
response_for(IdStrs) ->
    Ids = lists:map(fun str_to_id/1, IdStrs),
    RootId = lists:last(Ids),
    io:format("RootId = ~p~n", [RootId]),    
    [
        "<HTML>",
        "<HEAD>",
        "   <TITLE>Item ", escape(id_to_str(RootId)), "</TITLE>",
        "</HEAD>",
        "<BODY>",
        "<DIV id='breadcrumbs'>",
        lists:map(fun(I) -> [": ", link(id_to_str(I), I)] end, Ids),
        "</DIV>",
        html_tree(1, lathos:subtree(RootId)),
        "</BODY>"
    ].
    
event_handler({get, _Hostname, Uri, _Args}, State) ->
    Response = case string:tokens(Uri, "/") of
        [] -> response_for(["index.0"]);
        Ids -> response_for(Ids)
    end,
    {[header({ok, html}), Response], State};
    
event_handler({post, _Hostname, _Uri, _Args}, State) ->
    Code = 404,
    Response = "<html><body><h1>post not permitted (yet)</h1></body></html>",
    {[header({error,Code,Response})], State}.
