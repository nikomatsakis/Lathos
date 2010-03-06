-module(lathos_serve).
-include("lathos.hrl").
-import(pico_utils, [header/1, body/1, str/1]).
-export([start_handler/1, event_handler/2, stop_handler/2]).

start_handler(_) ->
    lathos:start(), 
    ok.
    
stop_handler(Reason, ok) ->
    lathos:stop(), 
    {Reason, ok}.
    
colors() -> ["CCCCCC", "BBBBBB", "AAAAAA", "999999"].
color(Depth) -> 
    Colors = colors(),
    lists:nth(Depth rem length(Colors), Colors).

escape(Text) -> pico_utils:quote_lt(Text). % XXX Do more.

id(Id) -> io_lib:format("~p.~p", [Id#id.name, Id#id.version]).
url(Id) -> pico_utils:str2urlencoded(id(Id)).

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
        " id='", id(Node#node.id), "'",
        " class='", "'",
        " style='background-color: '", color(Depth), "'",
        ">", newline(),
        "<SPAN class='msg' onclick='toggleId(\"", id(Node#node.id), "\")'>",
        html_description(Node#node.description),
        "</SPAN>", newline(),
        lists:map(fun(C) -> html_tree(Depth+1, C) end, Children),
        "</DIV>", newline()
    ].
    
response_for(Ids) ->
    RootId = lists:last(Ids),
    [
        "<HTML>",
        "<HEAD>",
        "   <TITLE>Item ", escape(id(RootId)), "</TITLE>",
        "</HEAD>",
        "<BODY>",
        "<DIV id='breadcrumbs'>",
        lists:map(fun(I) -> [": ", link(id(I), I)] end, Ids),
        "</DIV>",
        html_tree(0, RootId),
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
