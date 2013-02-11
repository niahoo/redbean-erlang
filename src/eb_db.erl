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
-export([load/2]).


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
        gproc:get_value({n,l,eb_db})
    catch
        error:badarg ->
            gproc:reg({n,l,eb_db}, default_eb_db),
            get_eb_db()
    end.


%% ===================================================================
%% API
%% ===================================================================

start_link(Adapter, Name, Conf, FMode) ->
    % error_logger:info_msg("Starting eb_db for ~p~n", [Name]),
    {ok, _Pid} = gen_server:start_link({local, Name}, ?MODULE, [Adapter, Conf, FMode], []).



%% Working with beans ------------------------------------------------


store({eb_bean, #bean{tainted=false}}=Bean) ->
    %% @todo check if processlist
    Bean;

store(Bean) ->
    {ok, NewBean} = store_bean(Bean),
    {ok, NewBean}.


store_bean(Bean) ->
    {ok, NewBean} = gen_server:call(?DB, {store,Bean}),
    {ok, NewBean}.


load(Type, ID) ->
    RecordQuery = #rsq{table=eb_utils:to_binary(Type), props=[{id, ID}]},
    % @todo transformer les row en bean
    case gen_server:call(?DB, {select_record, RecordQuery})
        of {ok, 1, Row} -> {ok, Row}
         ; {ok, 0, []}    -> {not_found, []}
         ; Any           -> Any
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
