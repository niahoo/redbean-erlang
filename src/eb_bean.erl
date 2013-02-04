-module(eb_bean).


-export([new/1]).
-export([get/2,set/3]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-define(DICT, orddict).

-record(bean, {type :: atom(),
               props=?DICT:new(),
               tainted=true :: boolean()
              }).

new(Type) when is_atom(Type) ->
    {eb_bean, #bean{type=Type}}.

get(Key, {eb_bean,Bean}) when is_atom(Key) ->
    case ?DICT:find(Key, Bean#bean.props)
        of error -> undefined
         ; Value -> Value
    end.

set(Key, Value, {eb_bean,Bean}) when is_atom(Key) ->
   NewProps = ?DICT:store(Key, Value, Bean#bean.props),
   {eb_bean,Bean#bean{props=NewProps}}.



-ifdef(TEST).
new_test() ->
    ?assertMatch({eb_bean, #bean{type=testb, tainted=true}}, eb_bean:new(testb)).

getset_test() ->
    Bean = eb_bean:new(testb),
    Bean2 = Bean:set(mykey,<<"My Value">>),
    ?assertEqual(Bean2:get(mykey),{ok, <<"My Value">>}).
-endif.
