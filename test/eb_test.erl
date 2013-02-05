-module(eb_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlbean/include/erlbean.hrl").


%% setup() ->
%%     ok = application:start(erlbean).
%% cleanup() ->
%%     ok = application:stop(erlbean).


ebsetup_epgsql_test() ->

        ?assertMatch({ok, "value"}, ((eb:dispense(any)):set(key, "value")):get(key)),

        ?assertMatch({ok, "value2"},
            ((eb:dispense(any)
                ):set([{key2, "value2"}])
                ):get(key2)
        ),

        ?assertMatch({ok, "value3"},
            ((eb:dispense(any)
                ):set([{key2, "value2"},{key3, "value3"}])
                ):get(key3)
        ).






