-module(eb_adapter_epgsql_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlbean/include/erlbean.hrl").
-define(setup(F), {setup, , F}).


pg_conf() ->
    [{user,"test"},
     {password,"test"},
     {host,"localhost"},
     {opts,[{database,"test"}]
     }
    ].

ebsetup_test_() ->
    [
        {"Epgsql Adapter can be started and has a registered name"
         " and Connect to a test database with credentials test:test",
            {setup, local, fun startapp/0, fun stopapp/1,
             fun(started) ->
                {ok, Pid} = eb:setup(epgsql, my_test_name,pg_conf()),
                ?_assertMatch(P when is_pid(P), Pid),
                ?_assert(erlang:is_process_alive(Pid)),
                ?_assertEqual(Pid, whereis(my_test_name))
             end
            }
        }
    ].

startapp() ->
    ok = application:start(erlbean),
    started.

stopapp(_) ->
    ok = application:stop(erlbean).



