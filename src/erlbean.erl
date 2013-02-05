-module(erlbean).

-export([start/0]).
-export([start_test_db/0]).

start() ->

    ok = case application:start(gproc)
        of ok -> ok
         ; {error, {already_started,gproc}} -> ok
         ; _Error -> {error, {not_started, gproc}}
    end,



    application:start(erlbean).


start_test_db() ->

    TestConf =
        [{user,"test"},
         {password,"test"},
         {host,"localhost"},
         {opts,[{database,"test"}]
         }],

    eb:setup(epgsql,TestConf).

