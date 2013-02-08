-module(eb_adapter_epgsql_test).


-compile([nowarn_unused_function]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlbean/include/erlbean.hrl").
-include_lib("erlbean/include/testcfg.hrl").


queries_test_() ->
    {inorder, [
        {"Adapter should be able to fire a query",
          setup, local, fun startapp/0, fun stopapp/1,
          fun(Pid) ->
            ?_assertMatch(
              {ok, _Columns, [{8}]},
              eb_adapter:exec(Pid, "SELECT 3 + 5;")
            )
          end
        }
        ,
        {"Adapter should be able to fire a query with typed bindings",
          setup, local, fun startapp/0, fun stopapp/1,
          fun(Pid) ->
            ?_assertMatch( {ok, _Columns, [{8}]}, eb_adapter:exec(Pid, "SELECT $1::integer + 5;", [3]))
          end
        },
        {"Adapter should be able to create a table with atom, list and binary",
          setup, local, fun startapp/0, fun stopapp/1,
          fun(Pid) ->
            q(Pid, "drop table IF EXISTS t_test_create_binary ;"),
            q(Pid, "drop table IF EXISTS t_test_create_atom ;"),
            q(Pid, "drop table IF EXISTS t_test_create_list ;"),
            {inorder ,[
              ?_assertMatch(
                {ok, _Columns, []},
                q(Pid, "select table_name from information_schema.tables where table_schema = 'public';")
              ),
              ?_assertMatch(
                ok,
                eb_adapter:create_table(Pid, <<"t_test_create_binary">>)
              ),
              ?_assertMatch(
                ok,
                eb_adapter:create_table(Pid, t_test_create_atom)
              ),
              ?_assertMatch(
                ok,
                eb_adapter:create_table(Pid, "t_test_create_list")
              ),
              ?_assertMatch(
                {ok, _Columns, [{<<"t_test_create_binary">>},{<<"t_test_create_atom">>},{<<"t_test_create_list">>}]},
                q(Pid, "select table_name from information_schema.tables where table_schema = 'public';")
              ),
              ?_assertMatch( %% WRONG TABLE NAME
                {error, {bad_table_name, Name}},
                eb_adapter:create_table(Pid, "t_te'st_create_list")
              ),
              ?_assertMatch(?PG_EMPTY_RET, q(Pid, "drop table t_test_create_binary;")),
              ?_assertMatch(?PG_EMPTY_RET, q(Pid, "drop table t_test_create_atom;")),
              ?_assertMatch(?PG_EMPTY_RET, q(Pid, "drop table t_test_create_list;"))
            ]}
          end
        }
        % ,
        % {"Adapter should be able to alter a table and create columns\n"
        %  "  or change their type",
        %   setup, local,
        %   fun () ->
        %     Pid = startapp()
        %     , q(Pid, "create table t_test_alter ();"),
        %     started
        %   end,
        %   fun (Pid) ->
        %     q(Pid, "drop table if exists t_test_alter;"),
        %     stopapp(Pid)
        %   end,
        %   fun(started) ->
        %     {inorder, [
        %       ?_assertMatch(
        %         {ok, _Columns, []},
        %         q(Pid, "select column_name, data_type from information_schema.columns where table_name='t_test_alter'")
        %       ),
        %       [
        %        ?_assertMatch(ok, eb_adapter:add_column(Pid, "eterm", "bytea")),
        %        ?_assertMatch(ok, eb_adapter:add_column(Pid, "alter table t_test_alter add id serial primary key"))
        %       ],
        %       ?_assertMatch(
        %         {ok, _Columns, [{<<"eterm">>,<<"bytea">>},{<<"id">>, <<"integer">>}]},
        %         q(Pid, "select column_name, data_type from information_schema.columns where table_name='t_test_alter'")
        %       ),
        %       ?_assertMatch(
        %         {ok, _Columns, []},
        %         q(Pid, "alter table t_test_alter alter column eterm type varchar(10)")
        %       ),
        %       ?_assertMatch(
        %         {ok, _Columns, [{<<"eterm">>,<<"character varying">>},{<<"id">>, <<"integer">>}]},
        %         q(Pid, "select column_name, data_type from information_schema.columns where table_name='t_test_alter'")
        %       )
        %     ]}
        %   end
        % }
    ]}.

q(Pid, Query) -> eb_adapter:exec(Pid, Query).


startapp() ->
    {ok, Pid} = eb_adapter:start_link(eb_adapter_epgsql, ?PGTESTCONF),
    Pid.

stopapp(Pid) ->
  catch eb_adapter:stop(Pid),
  ok.

