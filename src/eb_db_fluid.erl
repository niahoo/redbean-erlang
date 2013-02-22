%%% Ici on a uniquement la partie serveur du module eb_db, en mode
%%% fluid

-module(eb_db_fluid).

-include_lib("erlbean/include/erlbean.hrl").

-export([handle/3]).

handle({store, Bean}, _From, State) ->
    DBA = State#ebdb.dba,
    BeanType = Bean:type(),
    %% first check if table exists and create it if not
    case eb_adapter:table_exists(DBA, BeanType)
        of true ->
            ok
         ; false ->
            ok = eb_adapter:create_table(DBA, BeanType)
    end,

    %% Then check each column if exists and if type accepts value
    ok = Bean:fold(
        fun (id, _Val, Acc) -> %% skip id return previous Acc
               Acc;
            (_Key, _Val, {error, Error}) -> %% previous adapt failed, return error
               {error, Error};
            (Key, Val, ok) -> %% previous adapt ok, process next
               adapt_column(DBA, BeanType, Key, Val)
        end, ok),

    %% Store the Bean in database
    {ok, ID} = eb_adapter:update_record(DBA, to_binary(BeanType), Bean:'export/id'(), Bean:id()),

    %%

    PostUpdateBean = Bean:set(id,ID),
    {reply, {ok, PostUpdateBean:untaint()}, State};


handle({select_record, Table, WhereCLause, Bindings}, _From, State) ->
    DBA = State#ebdb.dba,
    Rs = eb_adapter:select_record(DBA, Table, WhereCLause, Bindings),
    Reply = case Rs
        of {error, {no_table, _Table}} -> {ok, 0, []}
         ; Any -> Any
    end,
    {reply, Reply, State};

handle({select_match, Table, Props}, _From, State) ->
    DBA = State#ebdb.dba,
    Rs = eb_adapter:select_match(DBA, Table, Props),
    Reply = case Rs
        of {error, {no_table, _Table}} -> {ok, 0, []}
         ; Any -> Any
    end,
    {reply, Reply, State};

handle({select_row, Query, Bindings}, _From, State) ->
    DBA = State#ebdb.dba,
    Rs = eb_adapter:select_row(DBA, Query, Bindings),
    Reply = case Rs
        of {error, {no_table, _Table}} -> {ok, 0, []}
         ; Any -> Any
    end,
    {reply, Reply, State};

% handle({select_record, RecordQuery}, _From, State) ->
%     %% ici en mode fluide, on va masquer l'erreur de table inexistante
%     %% en renvoyant simplement un recordcount 0


handle(_Request, _From, State) ->
    Reply = {error, {?MODULE, unknown_request, _Request}},
    {reply, Reply, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%% Adapt columns : create column if not exists or widen type
adapt_column(DBA, BeanType, Key, Val) ->
    ValType = eb_adapter:scan_type(DBA, Val),
    ColToList = atom_to_list(Key),
    case eb_adapter:column_exists(DBA, BeanType, Key)
        of false ->
            ok = eb_adapter:add_column(DBA, BeanType, ColToList, ValType)
         ; true  -> %% columns exists, check if we must widen
            CurrentColType = eb_adapter:get_type(DBA, BeanType, Key),
            Candidate = eb_adapter:scan_type(DBA, Val),
            % ?DBGTYPE(Key),
            case eb_adapter:accept_type(DBA, CurrentColType, Candidate)
                of true ->
                    ok
                 ; {false, NewType} ->
                    % ?DBGTYPE(NewType),
                    % ?DBGTYPE(CurrentColType),
                    % ?DBGTYPE(Candidate),
                    ok = eb_adapter:widen_column(DBA, BeanType, Key, NewType)
            end
    end.

to_binary(X) -> eb_utils:to_binary(X).
