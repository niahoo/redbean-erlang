%%% Main logic
-module(eb_db).

-include_lib("erlbean/include/erlbean.hrl").

-behaviour(gen_server).

-define(DB,get_eb_db()).


%% Global API
-export([get_eb_db/0]).

%% API
-export([start_link/4]).
-export([get_adapter/1]).
-export([store/1]).
-export([load/2,find/3]).
-export([get_col/2,get_rows/2]).


%% gen_server callbacks
-export([init/1,
     handle_call/3,
     handle_cast/2,
     handle_info/2,
     terminate/2,
     code_change/3]).



%% ===================================================================
%% Global API
%% ===================================================================

%% catch une exception enregistré on  renvoie le toolkit par defaut :
%% default_eb_db. Pour éviter une exception les fois suivantes, on
%% l'enregistre dans gproc
get_eb_db() ->
    try
        gproc:get_value({p,l,eb_db})
    catch
        error:badarg ->
            gproc:reg({p,l,eb_db}, default_eb_db),
            get_eb_db()
    end.


%% ===================================================================
%% API
%% ===================================================================

start_link(Adapter, Name, Conf, FMode) ->
    % error_logger:info_msg("Starting eb_db for ~p~n", [Name]),
    {ok, _Pid} = gen_server:start_link({local, Name}, ?MODULE, [Adapter, Conf, FMode], []).


%% Working with Rows -------------------------------------------------

%% With gel_col, eb_db assumes that the user selected only 1 columns
%% so the first column is picked if there are more
get_col(Query, Bindings) ->
    RecordSet = simple_select(Query, Bindings),
    case RecordSet
        of {ok, _X, Rows} ->
            Column = [V || [{_K,V}|_RowCols] <- Rows],
            {ok, Column}
         ; Any -> Any
    end.

get_rows(Query, Bindings) ->
    RecordSet = simple_select(Query, Bindings),
    case RecordSet
        of {ok, _X, Rows} -> {ok, Rows}
         ; Any -> Any
    end.

simple_select(Query, Bindings) ->
    %% comme on passe une requête complète, on envoie tout dans la
    %% where clause, comme ça l'API a définir reste simple pour des
    %% adapters NoSQL par exemple
    %% On regle le selectsql sur une liste vide pour qu'il ne soit pas
    %% remplacé par le select par défaut. Vu que Query contient
    %% logiquement une instruction select
    _RecordSet = gen_server:call(?DB, {select_row, Query, Bindings}).

%% Working with beans ------------------------------------------------

store(Bean) ->
    case Bean:tainted()
        of false -> Bean
         ; true  -> {ok, _NewBean} = store_bean(Bean)
    end.

store_bean(Bean) ->
    {ok, NewBean} = gen_server:call(?DB, {store,Bean}),
    {ok, NewBean}.

%% La fonction load doit renvoyer un bean et un seul bean, car l'ID
%% est censé être unique. On check donc qu'on ne récupère qu'un seul
%% bean de l'adapter. L'adpter pourrait en renvoyer plusieurs si
%% l'utilisateur fait n'importe quoi avec sa base de données
load(Type, ID) ->
    Props=[{<<"id">>, ID}],
    Table = eb_utils:to_binary(Type),
    RecordSet = gen_server:call(?DB, {select_match, Table, Props}),
    Bean = eb_bean:new(Type),
    case RecordSet
        of {ok, 1, [Row]} ->
                {ok, Bean2} = Bean:set(Row),
                {ok, Bean2:untaint()}
         ; {ok, 0, []}  ->
                {not_found, Bean}
         ; {ok, _X, Rows} when is_list(Rows) ->
                FirstRow = hd(Rows),
                {ok, Bean2} = Bean:set(FirstRow),
                {ok, Bean2:untaint()}
         ; Any ->
                Any
    end.

%% Ici on recherche des beans. Comme l'utilisateur peut insérer du SQL
%% qui vient après une where clause, i.e ORDER BY, LIMIT, GROUP BY, on
%% passe obligatoirement une props vide dans la #rsq
find(Type, WhereSQL, Bindings) ->
    Table = eb_utils:to_binary(Type),
    RecordSet = gen_server:call(?DB, {select_record, Table, WhereSQL, Bindings}),
    case RecordSet
        of {ok, _Count, Rows} ->
            Beans = convert_to_beans(Type, Rows),
            {ok, Beans}
         ; Any -> Any
    end.

%% Other -------------------------------------------------------------

get_adapter(Pid) ->
    gen_server:call(Pid,get_adapter).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%     {ok, State, Timeout} |
%%     ignore |
%%     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Adapter, Conf, FMode]) ->
    %% Selon l'adapter qu'on nous a donné, on trouve le bon module
    AdapterModule = erlbean:adapter_module(Adapter),

    % error_logger:info_msg("Starting dba for ~p~n", [process_info(self(), registered_name)]),

    {ok, Pid} = eb_adapter:start_link(AdapterModule, Conf),

    FModeModule = fmode_module(FMode),
    % error_logger:info_msg("Dba pid is ~p~n", [Pid]),

    {ok, #ebdb{dba=Pid, m=FModeModule}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%       {reply, Reply, State} |
%%       {reply, Reply, State, Timeout} |
%%       {noreply, State} |
%%       {noreply, State, Timeout} |
%%       {stop, Reason, Reply, State} |
%%       {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_adapter, _From, State) ->
    {reply, State#ebdb.dba, State};

handle_call(freeze, _From, State) ->
    {reply, ok, State#ebdb{m=fmode_module(production)}};

handle_call(AnyRequest, From, State) ->
    Module = State#ebdb.m,
    Module:handle(AnyRequest, From, State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%      {noreply, State, Timeout} |
%%      {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%       {noreply, State, Timeout} |
%%       {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% wrap(#bean{}=B) -> {eb_bean, B}.

fmode_module(fluid) -> eb_db_fluid.

convert_to_beans(Type, Rows) ->
    convert_to_beans(Type, Rows, []).

convert_to_beans(_Type, [], Acc) ->
    %% on renverse la liste au cas où l'utilisateur a soumis une
    %% clause ORDER BY
    lists:reverse(Acc);

convert_to_beans(Type, [Row|Rows], Acc) when is_atom(Type) ->
    {ok, Bean} = eb:proc([{dispense, Type},{set, Row}, untaint]),
    convert_to_beans(Type, Rows, [Bean|Acc]).


