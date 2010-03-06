-module(lathos_tests).
-include_lib("eunit/include/eunit.hrl").
-include("lathos.hrl").

start_stop_test() ->
    lathos:start(),
    lathos:stop().
