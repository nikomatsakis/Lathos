-module(lathos_tests).
-include_lib("eunit/include/eunit.hrl").
-include("lathos.hrl").

%start_stop_test() ->
%    lathos:start(),
%    lathos:stop().

empty_node_test() ->
    lathos:start(),
    lathos:reset(),
    {unlinked_ids, []} = lathos:create_node(node0, [], []),
    {node, #node{id=node0, parent_ids=[], description=[]}, []} = lathos:subtree(node0).
    
child_node_test() ->
    lathos:start(),
    lathos:reset(),
    {unlinked_ids, []} = lathos:create_node(node0, [], []),
    {unlinked_ids, []} = lathos:create_node(node00, [node0], []),
    {node, #node{id=node0, parent_ids=[], description=[]}, [
        {node, #node{id=node00, parent_ids=[node0], description=[]}, []}
    ]} = lathos:subtree(node0).
    
linked_node_test() ->
    lathos:start(),
    lathos:reset(),
    {unlinked_ids, [node1]} = lathos:create_node(node0, [], [
        {text, "See: "}, {link, "here", node1}]),
    {unlinked_ids, [node2]} = lathos:create_node(node1, [], [
        {text, "See: "}, {link, "here", node0}, {text, "and"}, {link, "here", node2}]),
    {node, #node{id=node0, parent_ids=[], description=[
        {text, "See: "}, {link, "here", node1}]}, []} = lathos:subtree(node0),
    {node, #node{id=node1, parent_ids=[], description=[
        {text, "See: "}, {link, "here", node0}, {text, "and"}, {link, "here", node2}]}, []} = lathos:subtree(node1).
        
    