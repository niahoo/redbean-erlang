
-module(erlbean_sup).

-behaviour(supervisor).

-export([start_link/0,
         start_toolkit/4
        ]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_toolkit(AdapterModule, Name, Conf, IsFrozen) ->
    % error_logger:info_msg("Starting toolkit ~p~n", [Name]),
    supervisor:start_child(?SERVER, [AdapterModule, Name, Conf, IsFrozen]).

init([]) ->

    %% la erlang toolbox doit juste lancer le start_link du module
    %% adapter, ce dernier doit s'enregistrer avec son Name

    Element = {erlbean_toolkit, {erlbean_toolkit, start_link, []},
               permanent, infinity, supervisor, [erlbean_toolkit]},
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 1, 10},
    % error_logger:info_msg("Supervisor ~p config done~n", [?MODULE]),
    {ok, {RestartStrategy, Children}}.

