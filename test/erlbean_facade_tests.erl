-module(erlbean_facade_tests).

-include_lib("eunit/include/eunit.hrl").

setup_test () ->
    ?assertEqual(r:setup(), {ok, amoispa}).
