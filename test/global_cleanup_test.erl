%%% These are not tests.
%%% Functions are executed before the testing start, and cleanup the
%%% test databases

-module(global_cleanup_test).


-compile([nowarn_unused_function]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlbean/include/erlbean.hrl").
-include_lib("erlbean/include/testcfg.hrl").

delete_all_test() ->
        {
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




q(Pid, Query) -> eb_adapter:exec(Pid, Query).
q(Pid, Query, Bindings) -> eb_adapter:exec(Pid, Query, Bindings).

startapp() ->
    {ok, Pid} = eb_adapter:start_link(eb_adapter_epgsql, ?PGTESTCONF),
    Pid.

stopapp(Pid) ->
  catch eb_adapter:stop(Pid),
  ok.

