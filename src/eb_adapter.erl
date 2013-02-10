-module(eb_adapter).

-export([behaviour_info/1]).
-behaviour(gen_server).

-export_type([dbatype/0]).
-type dbatype() :: integer | double | binary | text.

%% API
-export([start_link/2,stop/1]).
-export([exec/2,exec/3]).
-export([table_exists/2,create_table/2,get_tables/1]).
-export([quote/2,check/2,scan_type/2]).
-export([update_record/4]).
-export([column_exists/3,add_column/4,get_columns/2,widen_column/4]).

%% gen_server callbacks
-export([init/1,
     handle_call/3,
     handle_cast/2,
     handle_info/2,
     terminate/2,
     code_change/3]).

%% TEST EXPORT
-ifdef(TEST).
-export([get_state/1,set_state/2]).
-endif.

-record(state, {m, dbastate}).


behaviour_info(callbacks) ->
    [
     {init,1}, %% initialization of adapter state
     {quote, 2}, %% quote table and column names
     {scan_type, 2}, %% check a value and define the :: eb_adapter:dbatype() that must be used
     {check, 2}, %% check validity of a table/column name
     {exec, 2}, %% execute a query and eventually return values
     {create_table, 2},
     {get_tables, 2},
     {add_column, 2},
     {get_columns, 2},
     {widen_column, 2},
     {update_record, 2},
     {close, 1} %% close the connexion
    ];

behaviour_info(_Other) ->
    undefined.

%%%===================================================================
%%% API
%%%===================================================================


start_link(AdapterModule, Conf) ->
    {ok, _Pid} = gen_server:start_link(?MODULE, [AdapterModule, Conf], []).

quote(Pid, Name) ->
    gen_server:call(Pid, {quote, Name}).

check(Pid, {Object, Name}) ->
    gen_server:call(Pid, {check, {Object, Name}}).


%% tables ------------------------------------------------------------

table_exists(Pid, Table) ->
    {ok, Tables} = get_tables(Pid),
    lists:member(to_binary(Table),Tables).

create_table(Pid, Table) ->
    BinTable = to_binary(Table),
    case check(Pid, {table, BinTable})
        of true -> gen_server:call(Pid, {create_table, BinTable})
         ; false -> {error, {bad_table_name, BinTable}}
    end.

get_tables(Pid) ->
    gen_server:call(Pid, {get_tables, []}).

%% types -------------------------------------------------------------

scan_type(Pid, V) ->
    gen_server:call(Pid, {scan_type, V}).

%% columns -----------------------------------------------------------

column_exists(Pid, Table, Column) ->
    BinName = to_binary(Column),
    {ok, Columns} = get_columns(Pid,Table),
    lists:member(BinName, [Col || {Col,_Type} <- Columns]).

add_column(Pid, Table, Column, Type) ->
    TableExists = table_exists(Pid, Table),
    ColumnExists = column_exists(Pid, Table, Column),
    Check = check(Pid, {column, Column}),
    if not TableExists -> {error, {no_table, Table}}
     ; ColumnExists -> {error, {column_exists, {Table, Column}}}
     ; not Check -> {error, {bad_column_name, Column}}
     ; true -> gen_server:call(Pid, {add_column, {Table,Column,Type}})
    end.

%% doit retourner {ok, [{ColName,Type}]}.
get_columns(Pid, Table) ->
    gen_server:call(Pid, {get_columns, Table}).

widen_column(Pid, Table, Column, NewType) ->
    TableExists = table_exists(Pid, Table),
    ColumnExists = column_exists(Pid, Table, Column),
    if not TableExists -> {error, {no_table, Table}}
     ; not ColumnExists -> {error, {no_column, {Table, Column}}}
     ; true -> gen_server:call(Pid, {widen_column, {Table, Column, NewType}})
    end.

%% queries -----------------------------------------------------------

exec(Pid, Query) ->
    gen_server:call(Pid, {exec, Query}).

exec(Pid, Query, Bindings) ->
    gen_server:call(Pid, {exec, {Query, Bindings}}).

update_record(Pid, Table, KeyVals, ID) ->
    gen_server:call(Pid, {update_record, {Table, KeyVals, ID}}).

%% stop --------------------------------------------------------------

stop(Pid) ->
    gen_server:call(Pid, stop).

%%%===================================================================
%%% TEST API
%%%===================================================================

get_state(Pid) -> gen_server:call(Pid, get_state).
set_state(Pid, State) -> gen_server:call(Pid, {set_state, State}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([AdapterModule, Conf]) ->
    process_flag(trap_exit,true),
    {ok, DBAState} = AdapterModule:init(Conf),
    % error_logger:info_msg("Connection to postgres ok : ~p~n",[C]),
    {ok, #state{m=AdapterModule, dbastate=DBAState}}.

%% handle_call -------------------------------------------------------

handle_call(stop, _From, #state{m=M, dbastate=DS}=State) ->
    {stop, normal, State};

handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call({set_state, NewState}, _From, _State) ->
    {reply, ok, NewState};

%% ici c'est une q sans paramètres
handle_call({Fun, Req}, _From, #state{m=M, dbastate=DS}=State) ->
    try M:Fun(Req, DS)
        of {reply, Reply, NewDS} -> {reply, Reply, State#state{dbastate=NewDS}}
    catch
        nocatch:any -> fuck
    end.

%% handle_cast -------------------------------------------------------

handle_cast(_Msg, State) ->
    {noreply, State}.

%% handle_info -------------------------------------------------------

handle_info(_Info, State) ->
    {noreply, State}.

%% terminate ---------------------------------------------------------

terminate(normal, #state{m=M, dbastate=DS}) ->
    ok = M:close(DS);
% terminate(shutdown, #state{m=M, dbastate=DS}) ->
%     ok = M:close(DS);
terminate(Reason, _State) ->
    error_logger:error_msg("eb_adapter terminated with reason ~p~n", [Reason]),
    'KO'.

%% code_change -------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% tranforme les lists ou atoms en binary
to_binary(V) when is_atom(V) -> to_binary(atom_to_list(V)) ;
to_binary(V) when is_list(V) -> list_to_binary(V) ;
to_binary(V) when is_binary(V) -> V.
