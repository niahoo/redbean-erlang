
-define(DICT, orddict).
-define(DBGTYPE(Var), error_logger:info_msg("DEBUG: ~p:~p - ~p ~p()~n~n ~p ~n~n", [?MODULE, ?LINE, ??Var,eb_utils:typeof(Var), Var])).


-type dbatype() :: integer | double | binary | text.

-record(bean, { type :: atom(),
                props,
                tainted=true :: boolean()
              }).

-record(ebdb, { dba :: pid(),
                m :: atom()}).

%% recordset Query
-record(rsq, {  table :: binary(),
                props = [] :: [{Column :: binary(), Value :: term()}]
             }).



