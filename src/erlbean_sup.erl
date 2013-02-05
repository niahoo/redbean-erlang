
-module(erlbean_sup).

-behaviour(supervisor).

-export([start_link/0,
         add_database/3
        ]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

add_database(AdapterModule, Name, Conf) ->
    supervisor:start_child(?SERVER, [AdapterModule, Name, Conf]).

init([]) ->

    %% la erlang toolbox doit juste lancer le start_link du module
    %% adapter, ce dernier doit s'enregistrer avec son Name

    Element = {erlbean_kickstart, {erlbean_kickstart, start_link, []},
               permanent, 2000, worker, [erlbean_kickstart]},
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 1, 10},
    error_logger:info_msg("Supervisor ~p config done~n", [?MODULE]),
    {ok, {RestartStrategy, Children}}.

