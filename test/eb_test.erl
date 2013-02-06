-module(eb_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlbean/include/erlbean.hrl").
-include_lib("erlbean/include/testcfg.hrl").


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


%% @todo les toolkits devraient s'enregistrer, et avant de récup un
%% toolkit on devrait s'assurer qu'il soit lancé
get_toolkit_test_() ->
    {"Facade should get the default toolkit",
     setup, local, fun startapp/0, fun stopapp/1,
     [?_assertEqual(eb:get_toolkit(), default_toolkit)]
    }.


startapp() ->
    ok = application:start(erlbean),
    ok = application:start(gproc),
    eb:setup(epgsql,?PGTESTCONF),
    started.

stopapp(started) ->
    error_logger:tty(false),
    ok = application:stop(gproc),
    ok = application:stop(erlbean),
    error_logger:tty(true).

