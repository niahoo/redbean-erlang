
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
        % ,
        % {"If a bean is tainted, :store() should create a table",
        %     {
        %         setup, local,
        %         fun startapp/0,
        %         fun stopapp/1,
        %         fun (started) ->
        %             Bean = eb:dispense(bean),
        %             S = eb_adapter_epgsql,
        %             DBAState = S:get_state(dba()),
        %             {inorder, [
        %                 ?_assertNot(S:x_table_exists(<<"mytype">>, DBAState)),
        %                 ?_assertMatch({ok, _}, eb_db:store(Bean)),
        %                 ?_assert(S:x_table_exists(<<"mytype">>, DBAState))
        %             ]}
        %         end
        %     }
        % },
        % {"If a bean is tainted, :store() should make it not tainted",
        %     {
        %         setup, local,
        %         fun startapp/0,
        %         fun stopapp/1,
        %         fun (started) ->
        %             Bean = eb:dispense(bean),
        %             {ok, Bean2} = eb_db:store(Bean),
        %             {inorder, [
        %                 ?_assertNot(Bean2:tainted())
        %             ]}
        %         end
        %     }
        % }
    ].


startapp() ->
    ok = application:start(erlbean),
    eb:setup(epgsql,?PGTESTCONF),
    started.


stopapp(_) ->
    error_logger:tty(false),
    ok = application:stop(erlbean),
    error_logger:tty(true).

tk() -> eb_db:get_toolkit().
dba() -> eb_db:get_adapter(tk()).
