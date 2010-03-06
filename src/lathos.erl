-module(lathos).

%-include_lib("eunit/include/eunit.htl").
-include("lathos.hrl").
-behaviour(gen_server).
-export([start/0, stop/0, create_node/3, reset/0, subtree/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, { nodes, children_ids }).

%% ___ wrappers _________________________________________________________

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() ->
    gen_server:call(?MODULE, {stop}).
create_node(Id, Parent_ids, Description) ->
    gen_server:call(?MODULE, {create_node, Id, Parent_ids, Description}).
reset() ->
    gen_server:call(?MODULE, {reset}).
subtree(Id) ->
    gen_server:call(?MODULE, {subtree, Id}).

%% ___ gen_server routines ______________________________________________

init([]) -> 
    {ok, #state{
        nodes = ets:new(nodes, [set, private, {keypos, 2}]),
        children_ids = ets:new(children_ids, [bag, private])
    }}.

exists_node_named(Id, State) -> case ets:lookup(State#state.nodes, Id) of
    [] -> false;
    [_] -> true
end.

unlinked_ids([], _State) -> [];
unlinked_ids([{text, _} | Ids], State) -> unlinked_ids(Ids, State);
unlinked_ids([{link, _, Id} | Ids], State) ->
    case exists_node_named(Id, State) of
        false -> [Id | unlinked_ids(Ids, State)];
        true -> unlinked_ids(Ids, State)
    end.
    
insert_child(Child_id, State) -> 
    fun(Parent_id) -> ets:insert(State#state.children_ids, {Parent_id, Child_id}) end.

expand(Id, Visited0, State) ->
    case ets:lookup(State#state.nodes, Id) of
        [] -> {no_such_node, Id};
        [Node] ->
            VisitedN = sets:add_element(Id, Visited0),
            Children_tuples = ets:lookup(State#state.children_ids, Id),
            Children_ids0 = lists:map(fun({_, X}) -> X end, Children_tuples),
            Children_ids1 = lists:filter(fun(X) -> not(sets:is_element(X, VisitedN)) end, Children_ids0),
            Children = lists:map(fun(X) -> expand(X, VisitedN, State) end, Children_ids1),
            {node, Node, Children}
    end.

handle_call(
    {create_node, Id, Parent_ids, Description},
    _From, 
    State
) -> 
    case ets:lookup(State#state.nodes, Id) of
        [_Node] -> {reply, {duplicate_node, Id}, State};
        [] ->
            ets:insert(State#state.nodes, #node{id=Id, parent_ids=Parent_ids, description=Description}),
            lists:foreach(insert_child(Id, State), Parent_ids),
            Unlinked_ids = unlinked_ids(Description, State),
            {reply, {unlinked_ids, Unlinked_ids}, State}
    end;

handle_call(
    {reset},
    _From, 
    State
) -> 
    ets:delete_all_objects(State#state.nodes),
    ets:delete_all_objects(State#state.children_ids),
    {reply, {ok}, State};
    
handle_call(
    {stop},
    _From, 
    State
) -> 
    {stop, stop_requested, ok, State};
    
handle_call(
    {subtree, Id},
    _From, 
    State
) ->
    {reply, expand(Id, sets:new(), State), State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, State) -> 
    ets:delete(State#state.nodes),
    ets:delete(State#state.children_ids),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.










