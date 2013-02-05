-module(eb_db_test).


-include_lib("eunit/include/eunit.hrl").
-include_lib("erlbean/include/erlbean.hrl").



-define(TESTCONF, [{user,"test"},{password,"test"},{host,"localhost"},{opts,[{database,"test"}]}]).


% create_table_test_qzdqzd() ->
%     [
%         {"An empty bean should trigger table creation",
%             {setup, local, fun startapp/0, fun stopapp/1,
%              fun(started) ->
%                 ?_assert(erlang:is_process_alive(Pid)),
%                 Bean = eb:dispense(mybean),
%                 eb:store(Bean),
%                 ?_assertMatch(P when is_pid(P), Pid),
%                 ?_assertEqual(Pid, whereis(my_test_name))
%              end
%             }
%         }
%     ].

startapp() ->
    ok = application:start(erlbean),
    ok = application:start(gproc),
    eb:setup(epgsql,?TESTCONF),
    started.


stopapp(_) ->

    ok = application:stop(gproc),
    ok = application:stop(erlbean).



