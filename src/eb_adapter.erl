-module(eb_adapter).

-export([behaviour_info/1]).
-behaviour(gen_server).

%% API
-export([start_link/2,stop/1]).
-export([exec/2,exec/3]).
-export([table_exists/2,create_table/2,get_tables/1]).
-export([quote/2,check/2]).

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
    [{init,1},
     {quote, 2},
     {scan_type, 2},
     {check, 2},
     {exec, 2},
     {create_table, 2},
     {close, 1}
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

check(Pid, {table, Name}) ->
    gen_server:call(Pid, {check, {table, Name}}).

exec(Pid, Query) ->
    gen_server:call(Pid, {exec, Query}).

exec(Pid, Query, Bindings) ->
    gen_server:call(Pid, {exec, {Query, Bindings}}).


table_exists(Pid, Name) when is_atom(Name) ->
    table_exists(Pid, list_to_binary(atom_to_list(Name)));
table_exists(Pid, Name) when is_binary(Name) ->
    {ok, Tables} = get_tables(Pid),
    lists:member(Name,Tables).

create_table(Pid, Name) when is_atom(Name) ->
    create_table(Pid, atom_to_list(Name));
create_table(Pid, Name) when is_binary(Name) ->
    create_table(Pid, binary_to_list(Name));
create_table(Pid, Name) when is_list(Name) ->
    case check(Pid, {table, Name})
        of true -> gen_server:call(Pid, {create_table, Name})
         ; false -> {error, {bad_table_name, Name}}
    end.

get_tables(Pid) ->
    gen_server:call(Pid, {get_tables, []}).

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

