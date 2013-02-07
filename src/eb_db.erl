%%% Main logic
-module(eb_db).

-include_lib("erlbean/include/erlbean.hrl").

-behaviour(gen_server).




%% Global API
-export([get_toolkit/0]).

%% API
-export([start_link/4]).
-export([get_adapter/1]).
-export([store/1]).


%% gen_server callbacks
-export([init/1,
     handle_call/3,
     handle_cast/2,
     handle_info/2,
     terminate/2,
     code_change/3]).


-record(state, {dba}).


%% ===================================================================
%% Global API
%% ===================================================================

%% catch une exception enregistré on  renvoie le toolkit par defaut :
%% default_toolkit. Pour éviter une exception les fois suivantes, on
%% l'enregistre dans gproc
get_toolkit() ->
    try
        gproc:get_value({n,l,eb_toolkit})
    catch
        error:badarg ->
            gproc:reg({n,l,eb_toolkit}, default_toolkit),
            get_toolkit()
    end.


%% ===================================================================
%% API
%% ===================================================================

start_link(Adapter, Name, Conf, FMode) ->
    % error_logger:info_msg("Starting eb_db for ~p~n", [Name]),
    {ok, _Pid} = gen_server:start_link({local, Name}, ?MODULE, [Adapter, Conf, FMode], []).

get_adapter(Toolkit) ->
    gen_server:call(Toolkit,get_adapter).


%% regarde dans gproc le toolkit enregistré. Si pas de toolkit on


store({eb_bean, #bean{tainted=false}}=Wrapper) ->
    %% @todo check if processlist
    Wrapper;

store({eb_bean, #bean{}=Bean}=Wrapper) ->
    store_bean(Bean).


store_bean(#bean{}=Bean) ->
    Toolkit = get_toolkit(),
    Toolkit.


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
    AdapterModule = erlbean:adapter_module(Adapter, FMode),

    % error_logger:info_msg("Starting dba for ~p~n", [process_info(self(), registered_name)]),

    {ok, Pid} = AdapterModule:start_link(Conf),

    % error_logger:info_msg("Dba pid is ~p~n", [Pid]),

    {ok, #state{dba=Pid}}.

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
    {reply, State#state.dba, State};

handle_call(_Request, _From, State) ->
    Reply = {?MODULE, unknown_request},
    {reply, Reply, State}.

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


