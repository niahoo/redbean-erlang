-module(eb_adapter_epgsql).

-behaviour(eb_adapter).

-include_lib("epgsql/include/pgsql.hrl").
-include_lib("erlbean/include/erlbean.hrl").

%% eb_adapter callbacks
-export([init/1,
         quote/2,
         scan_type/2,
         check/2,
         exec/2,
         get_tables/1,
         create_table/2,
         get_columns/2,
         add_column/2,
         widen_column/2,
         update_record/2,
         select_record/2,
         accept_type/2,
         close/1
        ]).




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

quote(Name) ->
    ["\"", Name, "\""].

quote(Name, C) ->
    {reply, quote(Name), C}.

%% Quote un paramètre
pquote(X) ->
    ["'", X, "'"].

%% ===================================================================
%% SCHEMA MODIFICATIONS
%% ===================================================================

check({table, Name}, C) ->
    {ok, Re} = re:compile("^[a-z_][0-9a-z_]{0,62}$"),
    Reply = case re:run(Name,Re)
        of {match, _} -> true
         ; _ -> false
    end,
    {reply, Reply, C};

check({column, Name}, C) ->
    {ok, Re} = re:compile("^[a-z_][0-9a-z_]{0,62}$"), %% @todo regarder les restrictions pour les colonnes
    Reply = case re:run(Name,Re)
        of {match, _} -> true
         ; _ -> false
    end,
    {reply, Reply, C}.

create_table(Table, C) ->
    Reply = case q(C, ["CREATE TABLE ", Table, " (id SERIAL PRIMARY KEY);"])
        of {ok, _Columns, []} -> ok
         ; Any -> Any
    end,
    {reply, Reply, C}.

get_tables(C) ->
    {ok, _Columns, Tables} = q(C,
        "SELECT table_name FROM information_schema.tables
        WHERE table_schema = 'public';"),
    {reply, {ok, [T || {T} <- Tables]}, C}.


get_columns(Table, C) ->
    {ok, _Cols, Columns} = q(C,
        ["SELECT column_name, data_type "
        "FROM information_schema.columns WHERE table_name = ", pquote(Table), ";"]),
    TablesTypesStandard = [{ColName,pg2dbatype(Type)} || {ColName, Type} <- Columns],
    {reply, {ok, TablesTypesStandard}, C}.


add_column({Table, Column, Type}, C) ->
    PgType = dba2pgtype(Type),
    Reply = case q(C, ["alter table ", Table, " add ", Column," ", PgType, ";"])
        of {ok, [], []} -> ok
         %% input data must be ok
         %% ; {error, #error{code = <<"42P01">>}} -> {error, {no_table, Table}}
    end,
    {reply, Reply, C}.


widen_column({Table, Column, NewType}, C) ->
    PgType = dba2pgtype(NewType),
    Reply = case q(C, ["alter table ", Table, " alter column ", Column," TYPE ", PgType, ";"])
        of {ok, [], []} -> ok
         %% input data must be ok
         %% ; {error, #error{code = <<"42P01">>}} -> {error, {no_table, Table}}
         %% ; {error, #error{code = <<"42703">>}} -> {error, {no_column, {Table, Column}}}
    end,
    {reply, Reply, C}.

scan_type(V, C) -> {reply, scan_type(V), C}.

-spec scan_type(any()) -> dbatype().
scan_type (V) when is_integer (V) -> integer;
scan_type (V) when is_float   (V) -> double;
scan_type (V) when is_binary  (V) -> binary;
scan_type (V) when is_list    (V) -> text;
scan_type (V) -> throw({error, {unhandled_value, V}}).

-spec pg2dbatype(binary()) -> dbatype().
pg2dbatype(<<"integer">>) -> integer;
pg2dbatype(<<"text">>) -> text;
pg2dbatype(<<"bytea">>) -> binary.

-spec dba2pgtype(dbatype()) -> binary().
dba2pgtype(integer) -> <<"integer">>;
dba2pgtype(text) -> <<"text">>;
dba2pgtype(binary) -> <<"bytea">>.

accept_type(Info, C) -> {reply, accept_type(Info), C}.

-spec accept_type({CurrentType :: dbatype(), ValueType :: dbatype()}) -> true | {false, NewType :: dbatype()}.
accept_type({integer,text}) -> {false, text};
accept_type({text,integer}) -> true.

%% ===================================================================
%% RECORDS INSERT/UPDATE
%% ===================================================================

update_record({Table, KeyVals, undefined=_ID}, C) ->
    insert_record(Table, KeyVals, C);

update_record({_Table, _KeyVals, _ID}, C) ->
    {reply, {ok, kikoolol}, C}.

insert_record(Table, KeyVals, C) ->
    %% on append une chaine vide pour avoir la première virgule
    Keys = [""|[atom_to_list(K) || {K,_V} <- KeyVals]],
    Vals = [V || {_K,V} <- KeyVals],
    Columns = string:join(Keys,","),

    %% Quand on génère les marqueurs, ils sont en ordre décroissant,
    %% c'est pourquoi il faut renverser la liste des valeurs
    {_, Markers} = lists:foldl(
        fun(_V,{X, Dolls}) -> %% returns ["$n", ... "$2", "$1"]
            {X+1, [", $", integer_to_list(X+1)|Dolls]}
        end, {0,[]},Vals), %% ici Vals n'est utilisé que pour la longueur de la liste

    Q = ["INSERT INTO ", Table, " ( id " , Columns, " ) VALUES "
         "( DEFAULT ", Markers, " ) RETURNING id;"],
    {ok, _, _, [{ID}]} = q(C,Q, lists:reverse(Vals)),
    {reply, {ok, ID}, C}.


select_record(#rsq{table=Table, selectsql=SELECT}=RSQ, C) ->
    {WhereSQL, Bindings} = build_where_clause(RSQ),
    Q = case SELECT
        of undefined -> ["SELECT * FROM ", Table, WhereSQL, ";"]
         ; _  -> [SELECT, "  ", WhereSQL, ";"]
    end,
    Reply = case q(C,Q, Bindings)
        of {ok, ColumnsInfo, Rows} ->
            %% On a récupéré des rows, on doit renvoyer une proplist
            %%                   [{colname, Value}]
            {ok, length(Rows), read_rows(ColumnsInfo, Rows)}
         ; {error, #error{code = <<"42P01">>}} ->
            {error, {no_table, Table}}
         ; Any -> Any
    end,
    {reply, Reply, C}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

q(C, Query) ->
    % ?DBGTYPE(Query),
    Rep = pgsql:equery(C, Query),
    tty_db_if_error(Rep, Query),
    Rep.

q(C, Query, []) ->
    %% Le drier n'accepte pas les listes vides
    q(C, Query);

q(C, Query, Bindings) ->
    % ?DBGTYPE(Query),
    % ?DBGTYPE(Bindings),
    Rep = pgsql:equery(C, Query, Bindings),
    tty_db_if_error(Rep, Query),
    Rep.


tty_db_if_error({error, #error{severity=S, code=C, message=M, extra=X}}, Q) ->
    error_logger:warning_msg(
        "PostgreSQL returned an error ~n"
        "severity : ~s~n"
        "code :     ~s~n"
        "message :  ~s~n"
        "query :    ~s~n"
        "extra :    ~p~n~n~n"
        ,[S,C,M,Q,X]
    );

tty_db_if_error(_,_) -> ok.

%% SELECT FROM WHERE -------------------------------------------------

build_where_clause(#rsq{wheresql=WhereSQL,bindings=Bindings,props=Props}) ->
    %% On calcule à partir de quel chiffre les bindings des props vont
    %% commencer : si on a 2 bindings, les bindings des props doivent
    %% commencer à $3
    PropsMarkStart = length(Bindings) + 1,
    WhereKey = case WhereSQL
        of "" -> " WHERE True "
         ; _  -> " WHERE "
    end,
    {PropsWhereSQL, PropsBindings} = props_to_WHERE_statements(Props, PropsMarkStart),
    FinalSQL = [WhereKey," ",WhereSQL," ",PropsWhereSQL],
    FinalBindings = Bindings ++ PropsBindings,
    {FinalSQL, FinalBindings}.

props_to_WHERE_statements([], _PMS) ->
    {"", []};
props_to_WHERE_statements(Props, PropsMarkStart) ->
    props_to_WHERE_statements(Props, [], [], PropsMarkStart).

props_to_WHERE_statements([], SqlAcc, Bindings, _) ->
    {SqlAcc, Bindings};

props_to_WHERE_statements([{Key,Val}|Props], SqlAcc, Bindings, IMark) ->
    SqlPart = [" AND ", Key, " = $", integer_to_list(IMark), " "],
    props_to_WHERE_statements(Props, [SqlPart|SqlAcc], [Val|Bindings], IMark+1).


%% format recordset --------------------------------------------------


%% On reçoit les résultats d'un recordset epgsql, l'info sur les
%% colonnes et les tuples correspondant aux rows
read_rows(ColumnsInfo, Rows) ->
    BinNames = lists:map(fun(Col) -> Col#column.name end, ColumnsInfo),
    Names = lists:map(fun bin_to_atom/1, BinNames),
    read_rows(Names, Rows, []).


read_rows(_ColumnsInfo, [], Props) ->
    %% no more rows, return
    Props;

read_rows(Names, [Row|Rows], Props) ->
    RowProps = lists:zip(Names, tuple_to_list(Row)),
    read_rows(Names, Rows, [RowProps|Props]).


bin_to_atom(X) when is_binary(X) -> list_to_atom(binary_to_list(X)).
