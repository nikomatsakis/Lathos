Nonterminals term terms list tuple.
Terminals string int '{' '}' '[' ']' ',' atom.

Rootsymbol term.

term -> tuple                       : '$1'.
term -> list                        : '$1'.
term -> atom                        : {_, _, V} = '$1', V.
term -> string                      : {_, _, V} = '$1', V.
term -> int                         : {_, _, V} = '$1', V.

terms -> term                       : [ '$1' ].
terms -> term ',' terms             : [ '$1' | '$3' ].

list -> '[' ']'                     : [ ].
list -> '[' terms ']'               : '$2'.

tuple -> '{' terms '}'              : list_to_tuple('$2').

Erlang code.

-export([tokenize/1, tokenize_and_parse/1]).
%% tokenize: produces the expected tokens for this grammar
%% from a string.

tokenize_and_parse(Str) ->
    Tokens = tokenize(Str),
    parse(Tokens).

tokenize(Str) -> tokenize(Str, 1, []).

tokenize_int([D|T], N) when D >= $0, D =< $9 ->
    tokenize_int(T, N*10 + (D - $0));
tokenize_int(T, N) ->
    {T, N}.
    
tokenize_string([$" | T], S) ->
    {T, lists:reverse(S)};
tokenize_string([$\\, C | T], S) ->
    tokenize_string(T, [C|S]);
tokenize_string([C|T], S) ->
    tokenize_string(T, [C|S]).

tokenize_atom([C|T], S) when C >= $0, C =< $9 ->
    tokenize_atom(T, [C|S]);
tokenize_atom([C|T], S) when C >= $a, C =< $z ->
    tokenize_atom(T, [C|S]);
tokenize_atom([C|T], S) when C >= $A, C =< $Z ->
    tokenize_atom(T, [C|S]);
tokenize_atom([$_|T], S) ->
    tokenize_atom(T, [$_|S]);
tokenize_atom(T, S) ->
    {T, list_to_atom(lists:reverse(S))}.

% End of string:
tokenize([], Line, Tokens) ->
    lists:reverse([{'$end', Line} | Tokens]);

% Newlines:
tokenize([$\n, $\r | T], Line, Tokens) ->
    tokenize(T, Line + 1, Tokens);
tokenize([$\r, $\n | T], Line, Tokens) ->
    tokenize(T, Line + 1, Tokens);
tokenize([$\r | T], Line, Tokens) ->
    tokenize(T, Line + 1, Tokens);
tokenize([$\n | T], Line, Tokens) ->
    tokenize(T, Line + 1, Tokens);

% Other whitespace:
tokenize([C | T], Line, Tokens) when (C =:= $ ) or (C =:= $\t) ->
    tokenize(T, Line, Tokens);

% Integers start with digits:
tokenize([C | T1], Line, Tokens) when C >= $0, C =< $9 -> 
    {T2, Int} = tokenize_int(T1, C - $0),
    tokenize(T2, Line, [{int, Line, Int} | Tokens]);
    
% Strings start with quotes:
tokenize([$" | T1], Line, Tokens) ->
    {T2, String} = tokenize_string(T1, []),
    tokenize(T2, Line, [{string, Line, String} | Tokens]);
    
% Atoms start with letters:
tokenize([C | T1], Line, Tokens) when ((C >= $a) and (C =< $z)) or ((C >= $A) and (C =< $Z)) ->
    {T2, Atom} = tokenize_atom(T1, [C]),
    tokenize(T2, Line, [{atom, Line, Atom} | Tokens]);
    
% Everything else is considered punctuation:
tokenize([C | T], Line, Tokens) ->
    tokenize(T, Line, [{list_to_atom([C]), Line} | Tokens]).