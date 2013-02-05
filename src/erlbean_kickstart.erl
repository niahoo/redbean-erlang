-module(erlbean_kickstart).

-export([start_link/3]).

start_link(AdapterModule, Name, Conf) ->
    error_logger:info_msg("Toolkit Start ~s ~s~n", [Name, AdapterModule]),
    AdapterModule:start_link(Name,Conf).
