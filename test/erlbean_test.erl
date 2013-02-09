-module(erlbean_test).

-include_lib("erlbean/include/erlbean.hrl").
-include_lib("eunit/include/eunit.hrl").


erlbean_start_test_() ->
    {"Application erlbean can start",
     setup, local,
     fun() -> ok end,
     fun(ok) ->
        error_logger:tty(false),
        application:stop(erlbean),
        error_logger:tty(true)
     end,
     ?_assertEqual(ok, application:start(erlbean))
     }.
