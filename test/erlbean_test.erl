-module(erlbean_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("erlbean/include/erlbean.hrl").


erlbean_start_test_() ->
    {setup, local,
     fun() -> ok end,
     fun(ok) ->
        error_logger:tty(false),
        application:stop(erlbean),
        error_logger:tty(true)
     end,
     ?_assertEqual(ok, application:start(erlbean))
     }.
