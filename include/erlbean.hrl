
-define(DICT, orddict).


-record(bean, {type :: atom(),
               props,
               tainted=true :: boolean()
              }).

-record(ebdb, { dba :: pid(),
                m :: atom()}).
