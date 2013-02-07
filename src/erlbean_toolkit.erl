%%% Toolkit is a supervisor. It handles 2 workers:
%%% - db_db instance which handles logic.
%%% - specific adapter instance

-module(erlbean_toolkit).

-behaviour(supervisor).


-export([start_link/4]).

-export([init/1]).


start_link(Adapter, Name, Conf, FMode) ->
    supervisor:start_link(?MODULE, [Adapter, Name, Conf, FMode]).


init([Adapter, Name, Conf, FMode]) ->


    %% eb_db va s'enregistrer avec Name afin qu'il soit accessible de
    %% partout dans l'application. Attention, Name contient un nom de
    %% toolkit mais il référence un gen_server de type eb_db

    %% On est en one_for_all : quand l'adapter plante, on redémarre
    %% également eb_db puisque ce dernier garde le pid de l'adapter

    EB_DB = {eb_db, {eb_db, start_link, [Adapter, Name, Conf, FMode]},
               permanent, 5000, worker, [eb_db]},
    Children = [EB_DB],
    RestartStrategy = {one_for_all, 1, 10},
    % error_logger:info_msg("Supervisor ~p config done~n", [?MODULE]),
    {ok, {RestartStrategy, Children}}.

