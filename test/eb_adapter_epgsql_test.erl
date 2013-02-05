-module(eb_adapter_epgsql_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlbean/include/erlbean.hrl").



-define(TESTCONF, [{user,"test"},{password,"test"},{host,"localhost"},{opts,[{database,"test"}]}]).


ebsetup_test_() ->
    [
        {"Epgsql Adapter can be started and has a registered name"
         " and Connect to a test database with credentials test:test",
            {setup, local, fun startapp/0, fun stopapp/1,
             fun(started) ->
                {ok, Pid} = eb:setup(epgsql, my_test_name,?TESTCONF),
                ?_assertMatch(_P when is_pid(_P), Pid),
                ?_assertEqual(true, erlang:is_process_alive(Pid)),
                ?_assertEqual(Pid, whereis(my_test_name))
             end
            }
        }
    ].

simple_query_test_() ->
    [
        {"Adapter should be able to fire a query",
            {setup, local, fun startapp/0, fun stopapp/1,
             fun(started) ->
                ?_assertMatch({ok, _Columns, [{8}]},
                              eb_adapter_epgsql:exec(eb:get_toolkit(), "SELECT 3 + 5"))

             end
            }
        }
    ].

create_table_test_() ->
    [
        {"Adapter should be able to create a table",
            {setup, local, fun startapp/0, fun stopapp/1,
             fun(started) ->
                ?_assertEqual(1,1) %%@todo
             end
            }
        }
    ].


startapp() ->
    ok = application:start(erlbean),
    ok = application:start(gproc),
    eb:setup(epgsql,?TESTCONF),
    started.

stopapp(_) ->
    error_logger:info_msg("STOP STOP STOP~n"),
    ok = application:stop(gproc),
    ok = application:stop(erlbean).



