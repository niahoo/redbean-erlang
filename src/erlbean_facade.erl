-module(erlbean_facade).


-include_lib("erlbean/include/erlbean.hrl").

-export([setup/2,setup/3]).
-export([dispense/1]).
-export([store/1]).
-export([get_toolkit/0]).


setup(Adapter, Conf) ->
    setup(Adapter, default_toolkit, Conf).

setup(Adapter, Name, Conf) when is_atom(Adapter) ->
    LA = atom_to_list(Adapter),
    AdapterModule = list_to_atom("eb_adapter_" ++ LA),
    % error_logger:info_msg("Use of database adapter ~p~n",[AdapterModule]),
    erlbean_sup:add_database(AdapterModule, Name, Conf).

dispense(Type) -> eb_bean:new(Type).


store({eb_bean, #bean{tainted=false}}=Wrapper) ->
    Wrapper;

store({eb_bean, #bean{}=_Bean}) ->
    Toolkit = get_toolkit(),
    Toolkit.



%% regarde dans gproc le toolkit enregistré. Si pas de toolkit on
%% catch une exception enregistré on  renvoie le toolkit par defaut :
%% default_toolkit. Pour éviter une exception les fois suivantes, on
%% l'enregistre dans gproc
get_toolkit() ->
    try
        gproc:get_value({n,l,eb_toolkit})
    catch
        error:badarg ->
            gproc:reg({n,l,eb_toolkit}, default_toolkit),
            get_toolkit()
    end.


%% ===================================================================
%% INTERNAL
%% ===================================================================

