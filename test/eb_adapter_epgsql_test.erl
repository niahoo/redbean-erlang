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
        {"Adapter should be able to create a table with atoms only, list and binary",
          setup, local, fun startapp/0, fun stopapp/1,
          fun(Pid) ->
            q(Pid, "DROP TABLE IF EXISTS t_test_create_binary ;"),
            q(Pid, "DROP TABLE IF EXISTS t_test_create_binary ;"),
            q(Pid, "DROP TABLE IF EXISTS t_test_create_atom ;"),
            q(Pid, "DROP TABLE IF EXISTS t_test_create_list ;"),
            q(Pid, "DROP TABLE IF EXISTS t_test_alter ;"),
            q(Pid, "DROP TABLE IF EXISTS chapter ;"),
            q(Pid, "DROP TABLE IF EXISTS mytype ;"),
            q(Pid, "DROP TABLE IF EXISTS bean ;"),
            q(Pid, "DROP TABLE IF EXISTS book ;"),
            {inorder ,[
              ?_assertMatch(
                {ok, _Columns, []},
                q(Pid, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public';")
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
                eb_adapter:create_table(Pid, "t_test_create_list_numbers_123")
              ),
              %% WRONG TABLE NAMES
              ?_assertMatch({error, {bad_table_name, _}}, eb_adapter:create_table(Pid, 'contains\"quote')),
              ?_assertMatch({error, {bad_table_name, _}}, eb_adapter:create_table(Pid, 'contains-tr')),
              ?_assertMatch({error, {bad_table_name, _}}, eb_adapter:create_table(Pid, '1numberfirstpos')),
              ?_assertMatch({error, {bad_table_name, _}}, eb_adapter:create_table(Pid, 'contains$doll')),
              ?_assertMatch({error, {bad_table_name, _}}, eb_adapter:create_table(Pid, 'contains.dot')),
              ?_assertMatch({error, {bad_table_name, _}}, eb_adapter:create_table(Pid, 'héhéhéfrench')),
              ?_assertMatch({error, {bad_table_name, _}}, eb_adapter:create_table(Pid, 'noUPPERCASEallowed')),
              ?_assertMatch(
                {ok, _Columns, [{<<"t_test_create_atom">>},{<<"t_test_create_binary">>},{<<"t_test_create_list_numbers_123">>}]},
                q(Pid, "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public' order by table_name;")
              ),
              % ?_assertMatch( %% WRONG TABLE NAME
              %   {error, {bad_table_name, Name}},
              %   eb_adapter:create_table(Pid, "t_te'st_create_list")
              % ),
              ?_assertMatch(?PG_EMPTY_RET, q(Pid, "DROP TABLE t_test_create_binary;")),
              ?_assertMatch(?PG_EMPTY_RET, q(Pid, "DROP TABLE t_test_create_atom;")),
              ?_assertMatch(?PG_EMPTY_RET, q(Pid, "DROP TABLE t_test_create_list_numbers_123;"))
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
            q(Pid, "DROP TABLE if exists t_test_alter;"),
            stopapp(Pid)
          end,
          fun(Pid) ->
            {inorder, [
              ?_assertMatch(
                {ok, _Columns, []},
                q(Pid, "SELECT column_name, data_type FROM information_schema.columns WHERE table_name='t_test_alter'")
              ),
              %% ADD COLUMN
              ?_assertMatch(false, eb_adapter:column_exists(Pid, "t_test_alter", "eterm123")),
              ?_assertMatch(ok, eb_adapter:add_column(Pid, "t_test_alter", "eterm123", binary)),
              ?_assertMatch(ok, eb_adapter:add_column(Pid, "t_test_alter", "id", integer)),
              ?_assertMatch(true, eb_adapter:column_exists(Pid, "t_test_alter", "eterm123")),
              ?_assertMatch(
                {ok, [{<<"eterm123">>,binary},{<<"id">>, integer}]},
                eb_adapter:get_columns(Pid, "t_test_alter")
              ),
              %% ADD COLUMN WRONG COLUMN NAMES
              ?_assertMatch({error, {bad_column_name, _}}, eb_adapter:add_column(Pid, "t_test_alter", "contains\"quote", integer)),
              ?_assertMatch({error, {bad_column_name, _}}, eb_adapter:add_column(Pid, "t_test_alter", "contains-tr", integer)),
              ?_assertMatch({error, {bad_column_name, _}}, eb_adapter:add_column(Pid, "t_test_alter", "contains$doll", integer)),
              ?_assertMatch({error, {bad_column_name, _}}, eb_adapter:add_column(Pid, "t_test_alter", "contains.dot", integer)),
              ?_assertMatch({error, {bad_column_name, _}}, eb_adapter:add_column(Pid, "t_test_alter", "1numberfirstpos", integer)),
              ?_assertMatch({error, {bad_column_name, _}}, eb_adapter:add_column(Pid, "t_test_alter", "héhéhéfrench", integer)),
              ?_assertMatch({error, {bad_column_name, _}}, eb_adapter:add_column(Pid, "t_test_alter", "noUPPERCASEallowed", integer)),
              %% ADD COLUMN NON EXISTENT TABLE
              ?_assertMatch({error, {no_table, _}}, eb_adapter:add_column(Pid, "nonexistenttable", "good_name", integer)),
              %% WIDEN COLUMN
              ?_assertMatch(ok, eb_adapter:widen_column(Pid, "t_test_alter", "eterm123", text)),
              %% WIDEN CHECK
              ?_assertMatch(
                {ok, [{<<"eterm123">>,text},{<<"id">>, integer}]},
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
                {ok, _Columns, [{<<"eterm123">>,<<"text">>},{<<"id">>, <<"integer">>}]},
                q(Pid, "SELECT column_name, data_type FROM information_schema.columns WHERE table_name='t_test_alter'")
              )
              % , ?_assertMatch(?PG_EMPTY_RET, q(Pid, "DROP TABLE t_test_alter;"))
            ]}
          end
        },
        {"Adapter should be able to INSERT values",
          setup, local, fun startapp/0, fun stopapp/1,
          fun(Pid) ->
              q(Pid,"create table if not exists testint (id serial primary key, col_1 text, col_2 text)"),
            [
              ?_assertMatch(
                {ok,1},
                eb_adapter:exec(Pid,
                  "INSERT INTO testint (id, col_1, col_2) VALUES (DEFAULT, $1, $2)",
                  [123, "yo man"]
                )
              ),
              ?_assertMatch({ok,[],[]}, q(Pid, "DROP TABLE testint"))
            ]
          end
        }
    ]}.

q(Pid, Query) -> eb_adapter:exec(Pid, Query).
q(Pid, Query, Bindings) -> eb_adapter:exec(Pid, Query, Bindings).


startapp() ->
    {ok, Pid} = eb_adapter:start_link(eb_adapter_epgsql, ?PGTESTCONF),
    Pid.

stopapp(Pid) ->
  catch eb_adapter:stop(Pid),
  ok.

