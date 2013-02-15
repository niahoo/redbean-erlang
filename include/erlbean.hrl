
-define(DBGTYPE(Var), error_logger:info_msg("DEBUG: ~p:~p - ~p ~p()~n~n ~p ~n~n", [?MODULE, ?LINE, ??Var,eb_utils:typeof(Var), Var])).



-record(ebdb, { dba :: pid(),
                m :: atom()}).

%% recordset Query
-record(rsq, {  table :: binary(),
                props = [] :: [{Column :: binary(), Value :: term()}]
             }).

-type dbatype() :: integer | double | binary | text.
-type dbarow() :: [{Column :: binary(), Value :: term()}].
-type dbarows() :: [dbarow()].
-type rsq() :: #rsq{}.

