-module(eb_adapter_epgsql).

-behaviour(gen_server).
-behaviour(eb_adapter).

-include_lib("epgsql/include/pgsql.hrl").


%% API
-export([start_link/1,quote/1,check/1]).

%% gen_server callbacks
-export([init/1,
     handle_call/3,
     handle_cast/2,
     handle_info/2,
     terminate/2,
     code_change/3]).

%% eb_adapter callbacks
-export([exec/2,exec/3]).
-export([quote/1]).
-export([table_exists/2,create_table/2]).

%% TEST EXPORT
-ifdef(TEST).
-export([get_state/1,set_state/2,get_tables/1,x_table_exists/2]).
-export([x_create_table/2]).
-endif.

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

start_link(Conf) ->
    {ok, _Pid} = gen_server:start_link(?MODULE, [Conf], []).

quote(Name) -> [$" | Name] ++ [$"|[]].

check({table, Name}) ->
    {ok, Re} = re:compile("^[a-z_]{1,63}$"),
    case re:run(Name,Re)
        of {match, _} -> true
         ; _ -> false
    end.

%%%===================================================================
%%% SERVER API
%%%===================================================================


exec(Pid, Query) ->
    gen_server:call(Pid, {exec, Query}).

exec(Pid, Query, Bindings) ->
    gen_server:call(Pid, {exec, Query, Bindings}).


table_exists(Pid, Name) when is_atom(Name) ->
    table_exists(Pid, list_to_binary(atom_to_list(Name)));
table_exists(Pid, Name) when is_binary(Name) ->
    gen_server:call(Pid, {table_exists, Name}).

create_table(Pid, Name) when is_atom(Name) ->
    create_table(Pid, list_to_binary(atom_to_list(Name)));
create_table(Pid, Name) when is_binary(Name) ->
    gen_server:call(Pid, {create_table, Name}).

%%%===================================================================
%%% TEST API
%%%===================================================================

get_state(Pid) -> gen_server:call(Pid, get_state).
set_state(Pid, State) -> gen_server:call(Pid, {set_state, State}).

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
    Reply = q(C, Query),
    tty_db_if_error(Reply),
    {reply, Reply, State};

%% Query with bindings
handle_call({exec, Query, Bindings}, _From, #state{c=C}=State) ->
    Reply = q(C, Query, Bindings),
    tty_db_if_error(Reply),
    {reply, Reply, State};

handle_call({table_exists, Name}, _From, #state{c=C}=State) ->
    Reply = x_table_exists(Name, State),
    {reply, Reply, State};

handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call({set_state, NewState}, _From, _State) ->
    {reply, ok, NewState};

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

q(C, Query) -> pgsql:equery(C, Query).
q(C, Query, Bindings) -> pgsql:equery(C, Query, Bindings).


get_tables(State) ->
    {ok, _Columns, Tables} = q(State#state.c,
        "select table_name from information_schema.tables
        where table_schema = 'public'"),
    {ok, [T || {T} <- Tables]}.

x_table_exists(Table, State) ->
    {ok, Tables} = get_tables(State),
    lists:member(Table,Tables).

x_create_table(Name, State) when is_binary(Name)->
    x_create_table(binary_to_list(Name), State);

x_create_table(Name, #state{c=C}) when is_list(Name) ->
    true = check({table,Name}),
    {ok, _Columns, []} = q(C, "create table " ++ Name ++ " (id SERIAL PRIMARY KEY);"),
    ok.



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
