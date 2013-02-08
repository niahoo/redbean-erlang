%%% Ici on a uniquement la partie serveur du module eb_db, en mode
%%% fluid

-module(eb_db_fluid).

-include_lib("erlbean/include/erlbean.hrl").

-export([handle/3]).

handle({store, Bean}, _From, State) ->
    %% first check if table exists and create it if not
    DBA = State#ebdb.dba,
    % case eb_adapter:table_exists(DBA, Bean:type())
    %     of true -> ok
    %      ; false ->
    %         {ok, _, _} =
    {reply, {ok, Bean}, State};

handle(_Request, _From, State) ->
    Reply = {?MODULE, unknown_request},
    {reply, Reply, State}.

