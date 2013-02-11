
%%% !! Test fuid mode

-module(eb_db_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlbean/include/erlbean.hrl").
-include_lib("erlbean/include/testcfg.hrl").


ebsetup_test_() ->
    [
        {"Epgsql Adapter can be started and has a registered name\n"
         " and Connect to a test database with credentials test:test",
            {setup, local, fun startapp/0, fun stopapp/1,
             fun(started) ->
                {ok, SupPid} = eb:setup(epgsql, my_test_name, ?PGTESTCONF), %% spawn du toolkit supervisor
                ?_assertMatch(_P when is_pid(_P), SupPid), %% test du toolkit supervisor
                ?_assertEqual(true, erlang:is_process_alive(SupPid)), %% test du toolkit supervisor
                %% ici my_test_name a été enregistré par eb_db, pas par le superviseur
                ?_assertEqual(true, lists:member(my_test_name, erlang:registered(my_test_name))),
                ?_assertNot(SupPid == whereis(my_test_name))
             end
            }
        }
    ].


internals_test_() ->
    [
        {"If a bean is not tainted, :store() simply return it",
            {
                setup, local,
                fun startapp/0,
                fun stopapp/1,
                fun (started) ->
                    {eb_bean, Bean} = eb:dispense(bean),
                    Bean2 = Bean#bean{tainted=false},
                    {eb_bean, Bean3} = eb:store({eb_bean, Bean2}),
                    {inorder, [
                        ?_assertMatch(Bean2, Bean3)
                    ]}
                end
            }
        }
        ,
        {"If a bean is tainted, :store() should create a table",
            {
                setup, local,
                fun startapp/0,
                fun stopapp/1,
                fun (started) ->
                    Bean = eb:dispense(mytype),
                    {ok, _Bean2} = eb:store(Bean),
                    {inorder, [
                        ?_assert(eb_adapter:table_exists(dba(),mytype)),
                        ?_assertMatch({ok, [], []},eb_adapter:exec(dba(),"drop table mytype"))
                    ]}
                end
            }
        },
        {"If a bean is tainted, :store() should make it not tainted",
            {
                setup, local,
                fun startapp/0,
                fun stopapp/1,
                fun (started) ->
                    Bean = eb:dispense(bean),
                    {ok, Bean2} = eb_db:store(Bean),
                    {inorder, [
                        ?_assertNot(Bean2:tainted()),
                        ?_assertMatch({ok, [], []},eb_adapter:exec(dba(),"drop table bean"))
                    ]}
                end
            }
        }
        ,
        {"A new bean should have a value undefined, store it should\n"
         "populate the id whit an integer",
            {
                setup, local,
                fun startapp/0,
                fun stopapp/1,
                fun (started) ->
                    Bean = eb:dispense(bean),
                    {ok, Bean2} = eb_db:store(Bean),
                    {inorder, [
                        ?_assertMatch({ok, undefined}, Bean:get(id)),
                        ?_assertMatch({ok, ID} when is_integer(ID), Bean2:get(id)),
                        ?_assertMatch({ok, [], []},eb_adapter:exec(dba(),"drop table bean"))
                    ]}
                end
            }
        },
        {"Create a bean should create columns",
            {
                setup, local,
                fun startapp/0,
                fun stopapp/1,
                fun (started) ->
                    {ok, Bean} = (eb:dispense(mytype)):set([{col_1, 123}, {col_2, "val II"}]),
                    {ok, _Bean2} = eb:store(Bean),
                    {ok,Cols} = eb_adapter:get_columns(dba(),mytype),
                    {inorder, [
                        ?_assertMatch([{<<"col_1">>,integer},{<<"col_2">>,text},{<<"id">>,integer}],lists:sort(Cols))
                    ]}
                end
            }
        }
        ,
        {"Update a bean and change type should change column type",
            {
                setup, local,
                fun startapp/0,
                fun stopapp/1,
                fun (started) ->
                    {ok, Bean} = (eb:dispense(mytype)):set(
                        [{col_1, "this is text"}, %% text in an integer column
                         {col_2, 321}             %% integer in a text column
                    ]),
                    {ok, _Bean2} = eb:store(Bean),
                    {ok, Cols} = eb_adapter:get_columns(dba(),mytype),
                    {inorder, [
                        ?_assertMatch([{<<"col_1">>,text},{<<"col_2">>,text},{<<"id">>,integer}],lists:sort(Cols))
                    ]}
                end
            }
        }
        ,
        {"Load a bean should get the values from the database, if the bean exists"
         " else a new bean is returned",
            {
                setup, local,
                fun startapp/0,
                fun stopapp/1,
                fun (started) ->
                    {ok, Bean} = (eb:dispense(mytype)):set(
                        [{name, "Ruben Calderon"},
                         {age, 45}
                    ]),
                    {ok, Bean2} = eb:store(Bean),
                    ID = Bean2:id(),
                    Wrap2 = eb:load(mytype,ID+20),
                    {inorder, [
                        % ?_assertMatch({ok, 45}, BeanL:get(age)),
                        % ?_assertMatch({ok, <<"Ruben Calderon">>}, BeanL:get(name)),
                        % ?_assertMatch({not_found, <<"Ruben Calderon">>}, BeanL:get(name)),
                        % ?_assertEqual(false, BeanL:tainted())
                        ?_assertMatch(ok, Wrap2)
                        ]}
                end
            }
        }
    ].


startapp() ->
    ok = application:start(erlbean),
    eb:setup(epgsql,?PGTESTCONF),
    started.


stopapp(_) ->
    error_logger:tty(false),
    ok = application:stop(erlbean),
    error_logger:tty(true).

db() -> eb_db:get_eb_db().
dba() -> eb_db:get_adapter(db()).
