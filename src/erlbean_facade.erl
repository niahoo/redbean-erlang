-module(erlbean_facade).


-export([setup/2,setup/3]).
-export([dispense/1]).


setup(Adapter, Conf) ->
    setup(Adapter, default_toolkit, Conf).

setup(Adapter, Name, Conf) when is_atom(Adapter) ->
    LA = atom_to_list(Adapter),
    AdapterModule = list_to_atom("eb_adapter_" ++ LA),
    error_logger:info_msg("Use of database adapter ~p~n",[AdapterModule]),
    erlbean_sup:add_database(AdapterModule, Name, Conf).

dispense(Type) -> eb_bean:new(Type).
