-module(eb_adapter).


-include_lib("erlbean/include/erlbean.hrl").

-behaviour(gen_server).

-record(state, {m, dbastate}).

%% API
-export([start_link/2,stop/1]).
-export([exec/2,exec/3]).
-export([table_exists/2,create_table/2,get_tables/1]).
-export([quote/2,check/2,scan_type/2]).
-export([update_record/4]).
-export([select_record/2]).
-export([column_exists/3,add_column/4,get_columns/2,widen_column/4,get_type/3,accept_type/3]).

%% gen_server callbacks
-export([init/1,
     handle_call/3,
     handle_cast/2,
     handle_info/2,
     terminate/2,
     code_change/3]).

-type dbareply(Reply) :: {reply, Reply, NewState :: term()}.
-type dbaerror() :: dbareply({error, Reason :: term()}).


%%%===================================================================
%%% eb_adapter Behaviour definition
%%%===================================================================


-callback init(Conf :: term()) -> {ok, State :: term()} | {error, Reason :: term()}.

-callback close(Sate :: term()) -> ok.

-callback exec(Query :: term() | {Query :: term(), Bindings :: term()}, State :: term()) ->
    dbareply(Result :: term()).

-callback quote(Name :: term(), State :: term()) ->
    dbareply(Quoted :: term()).

-callback scan_type(Value :: term(), State :: term()) ->
    dbareply(Type :: dbatype()).

-callback check({table | column, Name :: binary()}, State :: term()) ->
    dbareply(IsOk :: boolean()).

-callback create_table(Table :: binary(), State :: term()) ->
    dbareply(ok) | dbaerror().

-callback get_tables(State :: term()) ->
    dbareply({ok, [Table :: binary()]}).

-callback add_column({Table :: binary(),  Column :: binary(), Type :: dbatype()}, State :: term()) ->
    dbareply(ok) | dbaerror().

-callback get_columns(Table :: binary(), State :: term()) ->
    dbareply({ok, [{ColumnName :: binary(), Type :: dbatype()}]}).

-callback widen_column({Table :: binary(),  Column :: binary(), Type :: dbatype()}, State :: term()) ->
    dbareply(ok) | dbaerror().

-callback accept_type({CurrentType :: dbatype(), ValueType :: dbatype()}, State :: term()) ->
     dbareply(true | {false, NewType :: dbatype()}).

-callback update_record({Table :: binary(), KeyVals :: dbarow(), ID :: term()}, State :: term()) ->
    dbareply({ok, NewID :: term()}).

-callback select_record(RecordSetQuery :: rsq(), State :: term()) ->
    dbareply({ok, RecordCount :: pos_integer(), Rows :: [dbarow()]}).

%%%===================================================================
%%% TEST API
%%%===================================================================
-ifdef(TEST).
-export([get_state/1,set_state/2]).
get_state(Pid) -> gen_server:call(Pid, get_state).
set_state(Pid, State) -> gen_server:call(Pid, {set_state, State}).
-endif.

%%%===================================================================
%%% API
%%%===================================================================


start_link(AdapterModule, Conf) ->
    {ok, _Pid} = gen_server:start_link(?MODULE, [AdapterModule, Conf], []).

quote(Pid, Name) ->
    gen_server:call(Pid, {quote, Name}).

check(Pid, {Object, Name}) when not is_binary(Name) ->
    check(Pid, {Object, to_binary(Name)});
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
         ; false -> {error, {bad_table_name, Table}}
    end.

get_tables(Pid) ->
    gen_server:call(Pid, {get_tables}).

%% types -------------------------------------------------------------

scan_type(Pid, V) ->
    gen_server:call(Pid, {scan_type, V}).

%% columns/types------------------------------------------------------

column_exists(Pid, Table, Column) ->
    BinName = to_binary(Column),
    {ok, Columns} = get_columns(Pid, to_binary(Table)),
    lists:member(BinName, [Col || {Col,_Type} <- Columns]).

add_column(Pid, Table, Column, Type) ->
    BinTable = to_binary(Table),
    BinColumn = to_binary(Column),
    TableExists = table_exists(Pid, BinTable),
    ColumnExists = column_exists(Pid, BinTable, BinColumn),
    Check = check(Pid, {column, BinColumn}),
    if not TableExists -> {error, {no_table, BinTable}}
     ; ColumnExists -> {error, {column_exists, {BinTable, BinColumn}}}
     ; not Check -> {error, {bad_column_name, BinColumn}}
     ; true -> gen_server:call(Pid, {add_column, {BinTable,BinColumn,Type}})
    end.

%% doit retourner {ok, [{ColName,Type}]}.
get_columns(Pid, Table) ->
    gen_server:call(Pid, {get_columns, to_binary(Table)}).

widen_column(Pid, Table, Column, NewType) ->
    BinTable = to_binary(Table),
    BinColumn = to_binary(Column),
    TableExists = table_exists(Pid, BinTable),
    ColumnExists = column_exists(Pid, BinTable, BinColumn),
    if not TableExists -> {error, {no_table, BinTable}}
     ; not ColumnExists -> {error, {no_column, {BinTable, BinColumn}}}
     ; true -> gen_server:call(Pid, {widen_column, {BinTable, BinColumn, NewType}})
    end.

get_type(Pid, Table, Column) ->
    BinColumn = to_binary(Column),
    {ok, Columns} = get_columns(Pid, to_binary(Table)),
    case proplists:lookup(BinColumn, Columns)
        of {BinColumn, Type} -> Type
         ; none -> {error, {no_column, Column}}
    end.

%% @doc Ask the adapter if the column type CurrentColType accepts
%% values of the type Candidate.
%% The adapter function must return 'true' or {false, NewType} where
%% NewType is a dbatype() which must accept the value
accept_type(_Pid, X, X) ->
     true; %% if types are the same, always accept
accept_type(Pid, CurrentColType, Candidate) ->
    gen_server:call(Pid, {accept_type, {CurrentColType, Candidate}}).

%% queries -----------------------------------------------------------

exec(Pid, Query) ->
    gen_server:call(Pid, {exec, Query}).

exec(Pid, Query, Bindings) ->
    gen_server:call(Pid, {exec, {Query, Bindings}}).

update_record(Pid, Table, KeyVals, ID) ->
    gen_server:call(Pid, {update_record, {Table, KeyVals, ID}}).

select_record(Pid, #rsq{table=Table}=RecordQuery) ->
    %% l'adapter spécifique peut renvoyer une erreur genre no_table
    %% ici on check le nom juste pour s'assurer qu'on envoie un nom
    %% valide et que l'erreur ne vient pas du code
    true = check(Pid, {table, Table}),
    gen_server:call(Pid, {select_record, RecordQuery}).

%% stop --------------------------------------------------------------

stop(Pid) ->
    gen_server:call(Pid, stop).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([AdapterModule, Conf]) ->
    case AdapterModule:init(Conf)
        of {ok, DBAState} -> {ok, #state{m=AdapterModule, dbastate=DBAState}}
         ; {stop, Reason} -> {stop, Reason}
    end.
    % error_logger:info_msg("Connection to postgres ok : ~p~n",[C]),


%% handle_call -------------------------------------------------------

handle_call(stop, _From, State) ->
    {stop, normal, State};

handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call({set_state, NewState}, _From, _State) ->
    {reply, ok, NewState};

%% requests without paramters sent as unary tuples
handle_call({Fun}, _From, #state{m=M, dbastate=DS}=State) ->
    try M:Fun(DS)
        of {reply, Reply, NewDS} -> {reply, Reply, State#state{dbastate=NewDS}}
    catch
        nocatch:any -> fuck
    end;
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
to_binary(X) -> eb_utils:to_binary(X).
