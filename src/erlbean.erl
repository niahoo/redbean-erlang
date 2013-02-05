-module(erlbean).

-export([start/0]).

start() ->


    TestConf =
        [{user,"test"},
         {password,"test"},
         {host,"localhost"},
         {opts,[{database,"test"}]
         }],


    application:start(erlbean)
    , eb:setup(epgsql,TestConf)
    .

