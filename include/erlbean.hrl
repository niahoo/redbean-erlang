
-define(DICT, orddict).


-record(bean, {type :: atom(),
               props=?DICT:new(),
               tainted=true :: boolean()
              }).

-record(ebdb, { dba :: pid(),
                m :: atom()}).
