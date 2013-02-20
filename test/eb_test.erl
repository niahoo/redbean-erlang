-module(eb_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlbean/include/erlbean.hrl").
-include_lib("erlbean/include/testcfg.hrl").


%% @todo les toolkits devraient s'enregistrer, et avant de récup un
%% toolkit on devrait s'assurer qu'il soit lancé
get_eb_db_test_() ->
    [ {"Facade should get the default eb_db name",
        setup, local, fun startapp/0, fun stopapp/1,
        [?_assertEqual(eb:get_eb_db(), default_eb_db)]
      }
    , {"Test of forwarding queries to adapter",
        setup, local, fun startapp/0, fun stopapp/1,
        fun(_) ->
            eb:proc([{dispense, qtest},{set, a, 34}, store]),
            eb:proc([{dispense, qtest},{set, a, 11}, store]),
            Ids = eb:exec("SELECT id FROM qtest"),
            ID2  = eb:exec("SELECT a FROM qtest WHERE id = $1", [2]),
            [ ?_assertMatch({ok, _ColInf, [{1},{2}]}, Ids)
            , ?_assertMatch({ok, _ColInf, [{11}]}, ID2)
            ]
        end
      }
    ].


startapp() ->
    application:start(erlbean),
    eb:setup(epgsql,?PGTESTCONF),
    started.

stopapp(started) ->
    error_logger:tty(false),
    ok = application:stop(erlbean),
    error_logger:tty(true).

