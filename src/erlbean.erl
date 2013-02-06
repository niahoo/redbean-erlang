-module(erlbean).

-export([start/0]).
-export([start_test_db/0]).

start() ->
    application:start(erlbean).


start_test_db() ->

    TestConf =
        [{user,"test"},
         {password,"test"},
         {host,"localhost"},
         {opts,[{database,"test"}]
         }],

    eb:setup(epgsql,TestConf).

