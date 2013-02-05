-module(eb_adapter_epgsql_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlbean/include/erlbean.hrl").
-define(setup(F), {setup, , F}).

ebsetup_test_() ->
    {"Epgsql Adapter can be started and has a registered name",
        {setup, local, fun startapp/0, fun stopapp/1,
         fun(started) ->
            {ok, Pid} = eb:setup(epgsql, my_test_name,[]),
            ?_assertMatch(P when is_pid(P), Pid),
            ?_assert(erlang:is_process_alive(Pid)),
            ?_assertEqual(Pid, whereis(my_test_name))
         end
        }
    }.

startapp() ->
    ok = application:start(erlbean),
    started.

stopapp(_) ->
    ok = application:stop(erlbean).



