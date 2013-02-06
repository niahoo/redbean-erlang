-module(eb_adapter_epgsql).

-behaviour(gen_server).
-behaviour(eb_adapter).

-include_lib("epgsql/include/pgsql.hrl").


%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
     handle_call/3,
     handle_cast/2,
     handle_info/2,
     terminate/2,
     code_change/3]).

%% eb_adapter callbacks
-export([store/2,exec/2,exec/3]).

-define(SERVER, ?MODULE).

-record(state, {c}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Name,Conf) ->
    {ok, _Pid} = gen_server:start_link({local, Name}, ?MODULE, [Conf], []).


store(_,_) -> fuck.
exec(Toolkit, Query) ->
    gen_server:call(Toolkit, {exec, Query}).

exec(Toolkit, Query, Bindings) ->
    gen_server:call(Toolkit, {exec, Query, Bindings}).



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
init([Conf]) ->
    % error_logger:info_msg("Connecting to postgres ~p ~n~n~n",[?MODULE]),
    Host     = proplists:get_value(host,    Conf, "localhost"),
    Username = proplists:get_value(user,    Conf),
    Password = proplists:get_value(password,Conf),
    Opts     = proplists:get_value(opts,    Conf, []),
    true = is_list(Opts),
    {ok, C}  = pgsql:connect(Host, Username, Password, Opts),
    % error_logger:info_msg("Connection to postgres ok : ~p~n",[C]),
    {ok, #state{c=C}}.

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

%% ici c'est une q sans paramètres
handle_call({exec, Query}, _From, #state{c=C}=State) ->
    Reply = pgsql:equery(C, Query),
    tty_db_if_error(Reply),
    {reply, Reply, State};

%% Query with bindings
handle_call({exec, Query, Bindings}, _From, #state{c=C}=State) ->
    Reply = pgsql:equery(C, Query, Bindings),
    tty_db_if_error(Reply),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
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


tty_db_if_error({error, #error{severity=S, code=C, message=M, extra=X}}) ->
    error_logger:warning_msg(
        "PostgreSQL returned an error ~n"
        "severity : ~s~n"
        "code :     ~s~n"
        "message :  ~s~n"
        "extra :    ~p~n~n~n"
        ,[S,C,M,X]
    );

tty_db_if_error(_) -> ok.
