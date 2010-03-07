-module(lathos_tests).
-include_lib("eunit/include/eunit.hrl").
-include("lathos.hrl").

%start_stop_test() ->
%    lathos:start(),
%    lathos:stop().

empty_node_test() ->
    lathos:start(),
    lathos:reset(),
    Node0 = #node{id=node0, parent_ids=[], description=[]},
    [] = lathos:create_nodes([Node0]),
    {tree, Node0, []} = lathos:subtree(node0).
    
child_node_test() ->
    lathos:start(),
    lathos:reset(),
    Node0 = #node{id=node0, parent_ids=[], description=[]},
    Node00 = #node{id=node00, parent_ids=[Node0#node.id], description=[]},
    [] = lathos:create_nodes([Node0]),
    [] = lathos:create_nodes([Node00]),
    {tree, Node0, [{tree, Node00, []}]} = lathos:subtree(node0).
    
linked_node_test() ->
    lathos:start(),
    lathos:reset(),
    Node0 = #node{id=node0, parent_ids=[], description=[
        {text, "See: "}, {link, "here", node1}]},
    Node1 = #node{id=node1, parent_ids=[], description=[
        {text, "See: "}, {link, "here", node0}, {text, "and"}, {link, "here", node2}]},
    [node1] = lathos:create_nodes([Node0]),
    [node2] = lathos:create_nodes([Node1]),
    {tree, Node0, []} = lathos:subtree(node0),
    {tree, Node1, []} = lathos:subtree(node1).
    
linked_nodes_test() ->
    lathos:start(),
    lathos:reset(),
    Node0 = #node{id=node0, parent_ids=[], description=[
        {text, "See: "}, {link, "here", node1}]},
    Node1 = #node{id=node1, parent_ids=[], description=[
        {text, "See: "}, {link, "here", node0}, {text, "and"}, {link, "here", node2}]},
    [node2] = lathos:create_nodes([Node0, Node1]),
    {tree, Node0, []} = lathos:subtree(node0),
    {tree, Node1, []} = lathos:subtree(node1).
        
    