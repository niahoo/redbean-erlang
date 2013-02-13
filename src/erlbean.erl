-module(erlbean).

-export([start/0]).
-export([start_test_db/0]).
-export([adapter_module/1]).

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
%% Si l'adapter demandé n'est pas référencé, on renvoie simplement
%% l'atom passé en paramètre, ainsi un utilisateur peut donner son
%% propre module adapter

adapter_module(epgsql) -> eb_adapter_epgsql;
adapter_module(Other) -> Other.

