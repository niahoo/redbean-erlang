
-define(DICT, orddict).


-record(bean, { type :: atom(),
                props,
                tainted=true :: boolean()
              }).

-record(ebdb, { dba :: pid(),
                m :: atom()}).

%% record Query
-record(rsq, {  table :: atom(),
                props}).



-define(DBGTYPE(Var), error_logger:info_msg("DEBUG: ~p:~p - ~p ~p()~n~n ~p ~n~n", [?MODULE, ?LINE, ??Var,eb_utils:typeof(Var), Var])).
