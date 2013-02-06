-module(eb_adapter_epgsql_test).

-compile([nowarn_unused_function]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlbean/include/erlbean.hrl").
-include_lib("erlbean/include/testcfg.hrl").



ebsetup_test_() ->
    [
        {"Epgsql Adapter can be started and has a registered name"
         " and Connect to a test database with credentials test:test",
            {setup, local, fun startapp/0, fun stopapp/1,
             fun(started) ->
                {ok, Pid} = eb:setup(epgsql, my_test_name,?PGTESTCONF),
                ?_assertMatch(_P when is_pid(_P), Pid),
                ?_assertEqual(true, erlang:is_process_alive(Pid)),
                ?_assertEqual(Pid, whereis(my_test_name))
             end
            }
        }
    ].

queries_test_() ->
    {inorder, [
        {"Adapter should be able to fire a query",
          setup, local, fun startapp/0, fun stopapp/1,
          fun(started) ->
            ?_assertMatch(
              {ok, _Columns, [{8}]},
              eb_adapter_epgsql:exec(eb:get_toolkit(), "SELECT 3 + 5;")
            )
          end
        },
        {"Adapter should be able to fire a query with typed bindings",
          setup, local, fun startapp/0, fun stopapp/1,
          fun(started) ->
            ?_assertMatch( {ok, _Columns, [{8}]}, eb_adapter_epgsql:exec(eb:get_toolkit(), "SELECT $1::integer + $2; ", [5,3]) )
          end
        },
        {"Adapter should be able to create and drop a table",
          setup, local, fun startapp/0, fun stopapp/1,
          fun(started) ->
            q("drop table IF EXISTS t_test_create ;"),
            {inorder ,[
              ?_assertMatch(
                {ok, _Columns, []},
                q("select table_name from information_schema.tables where table_schema = 'public';")
              ),
              ?_assertMatch(
                {ok, _Columns, []},
                q("create table t_test_create (id SERIAL PRIMARY KEY);")
              ),
              ?_assertMatch(
                {ok, _Columns, [{<<"t_test_create">>}]},
                q("select table_name from information_schema.tables where table_schema = 'public';")
              ),
              ?_assertMatch(?PG_EMPTY_RET, q("drop table t_test_create;"))
            ]}
          end
        },
        {"Adapter should be able to alter a table and create columns or change their type",
          setup, local,
          fun () ->
            started = startapp()
            , q("create table t_test_alter ();"),
            started
          end,
          fun (Started) ->
            q("drop table if exists t_test_alter;"),
            stopapp(Started)
          end,
          fun(started) ->
            {inorder, [
              ?_assertMatch(
                {ok, _Columns, []},
                q("select column_name, data_type from information_schema.columns where table_name='t_test_alter'")
              ),
              [
               ?_assertMatch({ok, _Columns, []},q("alter table t_test_alter add eterm bytea")),
               ?_assertMatch({ok, _Columns, []},q("alter table t_test_alter add id serial primary key"))
              ],
              ?_assertMatch(
                {ok, _Columns, [{<<"eterm">>,<<"bytea">>},{<<"id">>, <<"integer">>}]},
                q("select column_name, data_type from information_schema.columns where table_name='t_test_alter'")
              ),
              ?_assertMatch(
                {ok, _Columns, []},
                q("alter table t_test_alter alter column eterm type varchar(10)")
              ),
              ?_assertMatch(
                {ok, _Columns, [{<<"eterm">>,<<"character varying">>},{<<"id">>, <<"integer">>}]},
                q("select column_name, data_type from information_schema.columns where table_name='t_test_alter'")
              )
            ]}
          end
        }
    ]}.


startapp() ->
    ok = application:start(erlbean),
    ok = application:start(gproc),
    eb:setup(epgsql,?PGTESTCONF),
    started.

stopapp(_) ->
    error_logger:tty(false),
    ok = application:stop(gproc),
    ok = application:stop(erlbean),
    error_logger:tty(true).

q(Q) -> eb_adapter_epgsql:exec(eb:get_toolkit(), Q).
qb(Q, Bindings) -> eb_adapter_epgsql:exec(eb:get_toolkit(), Q, Bindings).




