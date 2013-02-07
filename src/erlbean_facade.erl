-module(erlbean_facade).


-include_lib("erlbean/include/erlbean.hrl").

-export([setup/2,setup/3,setup/4]).
-export([dispense/1]).
-export([store/1]).
-export([get_toolkit/0]).


%% Setup -------------------------------------------------------------

setup(Adapter, Conf) ->
    setup(Adapter, default_toolkit, Conf, fluid).

setup(Adapter, Conf, frozen) ->
    setup(Adapter, default_toolkit, Conf, fluid);

setup(Adapter, Name, Conf) ->
    setup(Adapter, Name, Conf, fluid).

setup(Adapter, Name, Conf, FMode) when is_atom(Adapter) ->
    erlbean_sup:start_toolkit(Adapter, Name, Conf, FMode).











dispense(Type) -> eb_bean:new(Type).


store(Wrapper) -> eb_db:store(Wrapper).


%% regarde dans gproc le toolkit enregistré. Si pas de toolkit on
%% catch une exception enregistré on  renvoie le toolkit par defaut :
%% default_toolkit. Pour éviter une exception les fois suivantes, on
%% l'enregistre dans gproc
get_toolkit() -> eb_db:get_toolkit().


%% ===================================================================
%% INTERNAL
%% ===================================================================

