-module(erlbean).

-export([start/0]).

start() ->


    TestConf = [
        {host, "localhost"},
        {user, "postgres"},
        {password, "manager"}
    ],


    application:start(erlbean)
    , eb:setup(epgsql,TestConf)
    .

