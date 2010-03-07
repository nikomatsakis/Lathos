-module(lathos_parse_tests).
-include_lib("eunit/include/eunit.hrl").
-import(lathos_parse, [parse/1, tokenize/1, tokenize_and_parse/1]).


tokenize_atoms_test() ->
    [{atom, 1, foo}, {atom, 1, bar}, {'$end', 1}] = 
        tokenize("foo bar").

tokenize_strings_test() ->
    [{string, 1, "foo"}, {string, 1, "bar"}, {'$end', 1}] = 
        tokenize("\"foo\" \"bar\"").

tokenize_escaped_strings_test() ->
    [{string, 1, "\"foo"}, {string, 1, "x\"y"}, {string, 1, "bar\""}, {'$end', 1}] = 
        tokenize("\"\\\"foo\" \"x\\\"y\" \"bar\\\"\"").

tokenize_tuple_test() ->
    [   
        {'{', 1}, {atom, 1, id}, {',', 1}, 
        {string, 2, "index"}, {',', 2}, 
        {int, 3, 0}, {'}', 3}, {'$end', 3}
    ] =
        tokenize("{id,\n\"index\",\n0}").

parse_tuple_test() ->
    {ok, {id, "index", 0}} = tokenize_and_parse("{id,\n\"index\",\n0}").

parse_empty_list_test() ->
    {ok, []} = tokenize_and_parse("[]").

parse_one_atom_list_test() ->
    {ok, [one_thing_in_here]} = tokenize_and_parse("[one_thing_in_here]").

parse_many_atoms_list_test() ->
    {ok, [a, list, with, atoms]} = tokenize_and_parse("[a, list, with, atoms]").

parse_tuple_list_test() ->
    {ok, [{node, [a, nested, list], 22, "and a string"}, {foo}]} = 
        tokenize_and_parse("[{node, [a, nested, list], 22, \"and a string\"}, {foo}]").

