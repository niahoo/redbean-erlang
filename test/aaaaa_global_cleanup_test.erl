%%% These are not tests.
%%% Functions are executed before the testing start, and cleanup the
%%% test databases

-module(aaaaa_global_cleanup_test).


-compile([nowarn_unused_function]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlbean/include/erlbean.hrl").
-include_lib("erlbean/include/testcfg.hrl").

delete_all_test_() ->
        {
          setup, local, fun startapp/0, fun stopapp/1,
          fun(Pid) ->

            error_logger:info_msg("~n ------------ GLOBAL CLEANUP ------------~n~n"),

            %% DROP all test tables

            q(Pid, "DROP TABLE IF EXISTS t_test_create_binary ;"),
            q(Pid, "DROP TABLE IF EXISTS t_test_create_binary ;"),
            q(Pid, "DROP TABLE IF EXISTS t_test_create_atom ;"),
            q(Pid, "DROP TABLE IF EXISTS t_test_create_list ;"),
            q(Pid, "DROP TABLE IF EXISTS t_test_alter ;"),
            q(Pid, "DROP TABLE IF EXISTS colqtest ;"),
            q(Pid, "DROP TABLE IF EXISTS coltest ;"),
            q(Pid, "DROP TABLE IF EXISTS chapter ;"),
            q(Pid, "DROP TABLE IF EXISTS mytype ;"),
            q(Pid, "DROP TABLE IF EXISTS qtest ;"),
            q(Pid, "DROP TABLE IF EXISTS bean ;"),
            q(Pid, "DROP TABLE IF EXISTS book ;"),

            %% Return a fake test suite
            []
          end
        }.




q(Pid, Query) -> eb_adapter:exec(Pid, Query).
q(Pid, Query, Bindings) -> eb_adapter:exec(Pid, Query, Bindings).

startapp() ->
    {ok, Pid} = eb_adapter:start_link(eb_adapter_epgsql, ?PGTESTCONF),
    Pid.

stopapp(Pid) ->
  catch eb_adapter:stop(Pid),
  ok.

