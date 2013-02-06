-module(erlbean_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    % error_logger:info_msg("Erlbean starting~n"),
    erlbean_sup:start_link().

stop(_State) ->
    % error_logger:info_msg("Erlbean stopping~n"),
    ok.
