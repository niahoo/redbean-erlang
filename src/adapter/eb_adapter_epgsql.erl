-module(eb_adapter_epgsql).

-behaviour(eb_adapter).

-include_lib("epgsql/include/pgsql.hrl").

%% eb_adapter callbacks
-export([init/1,close/1,quote/2,scan_type/2,check/2,exec/2,get_tables/2,create_table/2]).




init(Conf) ->
    % error_logger:info_msg("Connecting to postgres ~p ~n~n~n",[?MODULE]),
    Host     = proplists:get_value(host,    Conf, "localhost"),
    Username = proplists:get_value(user,    Conf),
    Password = proplists:get_value(password,Conf),
    Opts     = proplists:get_value(opts,    Conf, []),
    true = is_list(Opts),
    {ok, C}  = pgsql:connect(Host, Username, Password, Opts),
    % error_logger:info_msg("Connection to postgres ok : ~p~n",[C]),
    {ok, C}.

close(C) ->
    % error_logger:info_msg("Closing pgsql connexion --------------- ! ~n"),
    ok = pgsql:close(C).




%% Query with bindings
exec({Query, Bindings}, C) ->
    Reply = q(C, Query, Bindings),
    {reply, Reply, C};

%% ici c'est une q sans paramètres
exec(Query, C) ->
    Reply = q(C, Query),
    {reply, Reply, C}.


quote(Name, C) ->
    Quoted = [$" | Name] ++ [$"|[]],
    {reply, Quoted, C}.

scan_type(Data, C) -> fuck.

check({table, Name}, C) ->
    {ok, Re} = re:compile("^[a-z_]{1,63}$"),
    Reply = case re:run(Name,Re)
        of {match, _} -> true
         ; _ -> false
    end,
    {reply, Reply, C}.

create_table(Name, C) ->
    Reply = case q(C, ["create table ", Name, " (id SERIAL PRIMARY KEY);"])
        of {ok, _Columns, []} -> ok
         ; Any -> Any
    end,
    {reply, Reply, C}.

get_tables(_, C) ->
    {ok, _Columns, Tables} = q(C,
        "select table_name from information_schema.tables
        where table_schema = 'public'"),
    {reply, [T || {T} <- Tables], C}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

q(C, Query) ->
    Rep = pgsql:equery(C, Query),
    tty_db_if_error(Rep),
    Rep.

q(C, Query, Bindings) ->
    Rep = pgsql:equery(C, Query, Bindings),
    tty_db_if_error(Rep),
    Rep.


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
