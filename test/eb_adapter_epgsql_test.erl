-module(eb_adapter_epgsql_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlbean/include/erlbean.hrl").



-define(TESTCONF, [{user,"test"},{password,"test"},{host,"localhost"},{opts,[{database,"test"}]}]).
-define(PG_NO_RETURN, {ok,[],[]}).

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

queries_test_() ->
    [
        {"Adapter should be able to fire a query",
          setup, local, fun startapp/0, fun stopapp/1,
          fun(started) ->
            ?_assertMatch({ok, _Columns, [{8}]},
                            eb_adapter_epgsql:exec(eb:get_toolkit(), "SELECT 3 + 5;"))

          end
        },
        {"Adapter should be able to create and drop a table",
          setup, local, fun startapp/0, fun stopapp/1,
          fun(started) ->
            eb_adapter_epgsql:exec(eb:get_toolkit(), "drop table IF EXISTS t_test_1 ;"),
            {inorder ,[
              ?_assertMatch(
                {ok, _Columns, []},
                eb_adapter_epgsql:exec(
                  eb:get_toolkit(),
                  "select table_name from information_schema.tables where table_schema = 'public';")
              ),
              ?_assertMatch(
                {ok, _Columns, []},
                eb_adapter_epgsql:exec(
                  eb:get_toolkit(),
                  "create table t_test_1 (id SERIAL PRIMARY KEY);")
              ),
              ?_assertMatch(
                {ok, _Columns, [{<<"t_test_1">>}]},
                eb_adapter_epgsql:exec(
                  eb:get_toolkit(),
                  "select table_name from information_schema.tables where table_schema = 'public';")
              ),
              ?_assertMatch(
                ?PG_NO_RETURN,
                eb_adapter_epgsql:exec(
                  eb:get_toolkit(),
                  "drop table t_test_1 ;")
              )


            ]}
          end
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



