-module(lathos_serve).
-include("lathos.hrl").
-import(pico_utils, [header/1, body/1, str/1]).
-export([start/0, stop/0]).
-export([start_handler/1, event_handler/2, stop_handler/2]).

start()  -> pico_http_server:start(4999, 20, ?MODULE, [1,2,3]).
stop()   -> pico_http_server:stop(4999, foo1234).

start_handler(_) ->
    lathos:start(), 
    lathos:create_node(#id{name="index", version=1}, [], [{text, "Index"}]),
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

id_to_str(Id) -> lists:flatten(io_lib:format("~s.~p", [Id#id.name, Id#id.version])).
str_to_id(Str) ->
    {match, [_, {Name_s, Name_l}, {Ver_s, Ver_l}]} = re:run(Str, "^(.*)\\.([0-9]+)$"),
    Name = string:substr(Str, Name_s+1, Name_l), 
    {Version, _} = string:to_integer(string:substr(Str, Ver_s+1, Ver_l)),
    #id{name=Name, version=Version}.
    
url(Id) -> 
    IdStr = id_to_str(Id),
    pico_utils:str2urlencoded(IdStr).
    
css() ->
    "DIV.log {"
    "    border-width: thin;"
    "    border-style: solid;"
    "    margin-top: .1cm;"
    "    margin-bottom: .1cm;"
    "    margin-left: .3cm;"
    "    margin-right: 0cm;"
    "}"
    ".initiallyOpen {"
    "    opacity: 1.0;"
    "}"
    ".initiallyClosed {"
    "    opacity: 0.25;"
    "}"
    "A:hover {"
    "    text-decoration: underline;"
    "}"
    "A:link {"
    "    text-decoration: none;"
    "}"
    "A:visited {"
    "    text-decoration: none;"
    "}"
    .

script() ->
    "function toggleId(id)"
    "{"
    "    var target = document.getElementById(id);"
    "    var kids = target.childNodes;"
    "    var openedKids = false;"
    "    var closedKids = false;"
    "    for(var i = 0; (i < kids.length); i++) {"
    "        var kid = kids[i];"
    "        if("
    "            kid.className == 'log initiallyOpen' ||"
    "            kid.className == 'log initiallyClosed'"
    "        ) {"
    "            if(kid.style.display == 'none') {"
    "                kid.style.display = 'block';"
    "                openedKids = true;"
    "            } else {"
    "                kid.style.display = 'none';"
    "                closedKids = true;"
    "            }"
    "        }"
    "    }"
    "    "
    "    if(openedKids) {"
    "        target.style.opacity = 1.0;"
    "    } else if (closedKids) {"
    "        target.style.opacity = 0.25;                            "
    "    }"
    "}"
    .

link(Text,Id) -> ["<A href='", url(Id), "'>", escape(Text), "</A>"].

html_description(_IdStr, []) ->
    [];
html_description(IdStr, [{text, Text} | D]) ->
    [
        "<SPAN class='msg' onclick='toggleId(\"", IdStr, "\")'>",
        escape(Text),
        "</SPAN>"
        | html_description(IdStr, D)
    ];
html_description(IdStr, [{link, Text, Id} | D]) ->
    [link(Text, Id), html_description(IdStr, D)].

html_tree(Depth, {tree, Node, Children}) ->
    [
        "<DIV",
        " id='", id_to_str(Node#node.id), "'",
        " class='log initiallyOpen'",
        " style='background-color: #", color(Depth), ";'",
        ">\n",
        html_description(id_to_str(Node#node.id), Node#node.description),
        lists:map(fun(C) -> html_tree(Depth+1, C) end, Children),
        "</DIV>\n"
    ];
html_tree(Depth, {no_tree, Id}) ->
    IdStr = id_to_str(Id),
    [
        "<DIV",
        " id='", IdStr, "'",
        " class='log initiallyOpen'",
        " style='background-color: #", color(Depth), ";'",
        ">\n",
        "<b>Node ", escape(IdStr), " is not defined.</b>\n",
        "</DIV>\n"
    ].
    
response_for(IdStrs) ->
    Ids = lists:map(fun str_to_id/1, IdStrs),
    RootId = lists:last(Ids),
    io:format("RootId = ~p~n", [RootId]),    
    [
        "<HTML>\n",
        "<HEAD>\n",
        "   <TITLE>Item ", escape(id_to_str(RootId)), "</TITLE>\n",
        "   <STYLE>", css(), "</STYLE>\n",
        "   <SCRIPT type='text/javascript'>", script(), "</SCRIPT>\n",
        "</HEAD>\n",
        "<BODY>\n",
        "<DIV id='breadcrumbs'>\n",
        lists:map(fun(I) -> [": ", link(id_to_str(I), I)] end, Ids),
        "</DIV>\n",
        html_tree(1, lathos:subtree(RootId)),
        "</BODY>\n",
        "</HTML>"
    ].

validate_chunk({text, String}) when is_list(String) ->
    {text, String};
validate_chunk({link, String, Id}) when is_list(String), is_tuple(Id) ->
    VId = validate_id(Id),
    {link, String, VId};
validate_chunk(Chunk) ->
    throw({invalid, chunk, Chunk}).
    
validate_description(Description) when is_list(Description) ->
    lists:map(fun validate_chunk/1, Description);
validate_description(Description) ->
    throw({invalid, description, Description}).
    
validate_id(#id{name=Name, version=Version}) when is_list(Name), is_integer(Version) ->
    #id{name=Name, version=Version};
validate_id(Id) ->
    throw({invalid, id, Id}).
    
validate_node(#node{id=Id, parent_ids=Parent_ids, description=Description}) when is_list(Parent_ids) ->
    VId = validate_id(Id),
    VParent_ids = lists:map(fun validate_id/1, Parent_ids),
    VDescription = validate_description(Description),
    #node{id=VId, parent_ids=VParent_ids, description=VDescription};
validate_node(Node) ->
    throw({invalid, node, Node}).
    
create_validated_node(VNode) ->
    lathos:create_node(VNode#node.id, VNode#node.parent_ids, VNode#node.description).
    
event_handler({get, _Hostname, "/favicon.ico", _Args}, State) ->
    Code = "404 Not Found",
    Response = "<html><body>no such file, dude.</body></html>",
    {[header({error,Code,Response})], State};   
     
event_handler({get, _Hostname, Uri, _Args}, State) ->
    io:format("Uri:~p~n", [Uri]),
    Decoded = pico_utils:urlencoded2str(Uri),
    Response = case string:tokens(Decoded, "/") of
        [] -> response_for(["index.1"]);
        Ids -> response_for(Ids)
    end,
    {[header({ok, html}), Response], State};
    
event_handler({post, _Hostname, "/create_node", [{PostData, _}]}, State) ->
    {ok, Node} = lathos_parse:tokenize_and_parse(PostData),
    io:format("Node=~p~n", [Node]),
    VNode = validate_node(Node),
    Response = create_validated_node(VNode),
    {[header({ok, text}), io_lib:format("~p", [Response])], State}.
