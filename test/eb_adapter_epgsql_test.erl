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
        },
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
            q(Pid, "drop table IF EXISTS t_test_alter ;"),
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
              %% WRONG TABLE NAMES
              ?_assertMatch({error, {bad_table_name, _}}, eb_adapter:create_table(Pid, "contains\"quote")),
              ?_assertMatch({error, {bad_table_name, _}}, eb_adapter:create_table(Pid, "contains-tr")),
              ?_assertMatch({error, {bad_table_name, _}}, eb_adapter:create_table(Pid, "contains$doll")),
              ?_assertMatch({error, {bad_table_name, _}}, eb_adapter:create_table(Pid, "contains.dot")),
              ?_assertMatch({error, {bad_table_name, _}}, eb_adapter:create_table(Pid, "héhéhéfrench")),
              ?_assertMatch({error, {bad_table_name, _}}, eb_adapter:create_table(Pid, "noUPPERCASEallowed")),
              ?_assertMatch(
                {ok, _Columns, [{<<"t_test_create_atom">>},{<<"t_test_create_binary">>},{<<"t_test_create_list">>}]},
                q(Pid, "select table_name from information_schema.tables where table_schema = 'public' order by table_name;")
              ),
              % ?_assertMatch( %% WRONG TABLE NAME
              %   {error, {bad_table_name, Name}},
              %   eb_adapter:create_table(Pid, "t_te'st_create_list")
              % ),
              ?_assertMatch(?PG_EMPTY_RET, q(Pid, "drop table t_test_create_binary;")),
              ?_assertMatch(?PG_EMPTY_RET, q(Pid, "drop table t_test_create_atom;")),
              ?_assertMatch(?PG_EMPTY_RET, q(Pid, "drop table t_test_create_list;"))
            ]}
          end
        },
        {"Adapter should be able to scan type of a value",
          setup, local, fun startapp/0, fun stopapp/1,
          fun(Pid) -> [
              ?_assertMatch(integer, eb_adapter:scan_type(Pid, 1545468756454)),
              ?_assertMatch(double,  eb_adapter:scan_type(Pid, 1.5)),
              ?_assertMatch(text,    eb_adapter:scan_type(Pid, "Hello, my name is erlbean")),

              %% A binary will be stored as binary
              ?_assertMatch(binary,  eb_adapter:scan_type(Pid, <<"Hello, my name is erlbean">>)),

              %% to store binary text, wrap it into a list
              ?_assertMatch(text,    eb_adapter:scan_type(Pid, [<<"Hello, my name is erlbean">>])),
              ?_assertMatch(text,    eb_adapter:scan_type(Pid, ["hello, ", [<<"my">>, "name"], [[[[[["is"]]]]]], <<"erlbean">>])),
              ?_assertMatch(integer, eb_adapter:scan_type(Pid, 1))
            ]
          end
        },
        {"Adapter should be able to alter a table and create columns\n"
         "  or change their type",
          setup, spawn,
          fun () ->
            Pid = startapp()
            , {ok, [], []} = q(Pid, "create table t_test_alter ();"),
            Pid
          end,
          fun (Pid) ->
            q(Pid, "drop table if exists t_test_alter;"),
            stopapp(Pid)
          end,
          fun(Pid) ->
            {inorder, [
              ?_assertMatch(
                {ok, _Columns, []},
                q(Pid, "select column_name, data_type from information_schema.columns where table_name='t_test_alter'")
              ),
              %% ADD COLUMN
              ?_assertMatch(false, eb_adapter:column_exists(Pid, "t_test_alter", "eterm")),
              ?_assertMatch(ok, eb_adapter:add_column(Pid, "t_test_alter", "eterm", binary)),
              ?_assertMatch(ok, eb_adapter:add_column(Pid, "t_test_alter", "id", integer)),
              ?_assertMatch(true, eb_adapter:column_exists(Pid, "t_test_alter", "eterm")),
              ?_assertMatch(
                {ok, [{<<"eterm">>,binary},{<<"id">>, integer}]},
                eb_adapter:get_columns(Pid, "t_test_alter")
              ),
              %% ADD COLUMN WRONG COLUMN NAMES
              ?_assertMatch({error, {bad_column_name, _}}, eb_adapter:add_column(Pid, "t_test_alter", "contains\"quote", integer)),
              ?_assertMatch({error, {bad_column_name, _}}, eb_adapter:add_column(Pid, "t_test_alter", "contains-tr", integer)),
              ?_assertMatch({error, {bad_column_name, _}}, eb_adapter:add_column(Pid, "t_test_alter", "contains$doll", integer)),
              ?_assertMatch({error, {bad_column_name, _}}, eb_adapter:add_column(Pid, "t_test_alter", "contains.dot", integer)),
              ?_assertMatch({error, {bad_column_name, _}}, eb_adapter:add_column(Pid, "t_test_alter", "héhéhéfrench", integer)),
              ?_assertMatch({error, {bad_column_name, _}}, eb_adapter:add_column(Pid, "t_test_alter", "noUPPERCASEallowed", integer)),
              %% ADD COLUMN NON EXISTENT TABLE
              ?_assertMatch({error, {no_table, _}}, eb_adapter:add_column(Pid, "nonexistenttable", "good_name", integer)),
              %% WIDEN COLUMN
              ?_assertMatch(ok, eb_adapter:widen_column(Pid, "t_test_alter", "eterm", text)),
              %% WIDEN CHECK
              ?_assertMatch(
                {ok, [{<<"eterm">>,text},{<<"id">>, integer}]},
                eb_adapter:get_columns(Pid, "t_test_alter")
              ),
              %% WIDEN COLUMN NON EXISTENT TABLE
              ?_assertMatch({error, {no_table, _}}, eb_adapter:widen_column(Pid, "nonexistenttable", "good_name", integer)),
              %% WIDEN COLUMN NON EXISTENT COLUMN
              ?_assertMatch({error, {no_column, _}}, eb_adapter:widen_column(Pid, "t_test_alter", "idontexist", integer)),
              %% WIDEN COLUMN BAD COLUMN NAME -- COULD NOT HAVE BEEN CREATED => no_column
              ?_assertMatch({error, {no_column, _}}, eb_adapter:widen_column(Pid, "t_test_alter", "noUPPERCASEallowed", integer)),
              %% CHECK COLUMNS
              ?_assertMatch(
                {ok, _Columns, [{<<"eterm">>,<<"text">>},{<<"id">>, <<"integer">>}]},
                q(Pid, "select column_name, data_type from information_schema.columns where table_name='t_test_alter'")
              ),
              ?_assertMatch(?PG_EMPTY_RET, q(Pid, "drop table t_test_alter;"))
            ]}
          end
        }
    ]}.

q(Pid, Query) -> eb_adapter:exec(Pid, Query).


startapp() ->
    {ok, Pid} = eb_adapter:start_link(eb_adapter_epgsql, ?PGTESTCONF),
    Pid.

stopapp(Pid) ->
  catch eb_adapter:stop(Pid),
  ok.

