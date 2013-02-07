-module(erlbean).

-export([start/0]).
-export([start_test_db/0]).
-export([adapter_module/2]).

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


%% Renvoie le nom du module adapter en fonction du nom de l'adapter,
%% ce qui permet d'indiquer epgsql au lieu de eb_adapter_epgsql dans
%% un eb:setup par exemple.

adapter_module(epgsql, fluid) -> eb_adapter_epgsql.

