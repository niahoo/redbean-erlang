-module(eb_bean).


-export([new/1]).
-export([get/2,set/2,set/3]).



-include_lib("erlbean/include/erlbean.hrl").



new(Type) when is_atom(Type) ->
    {eb_bean, #bean{type=Type}}.

get(Key, {eb_bean,Bean}) when is_atom(Key) ->
    case ?DICT:find(Key, Bean#bean.props)
        of error -> undefined
         ; Value -> Value
    end.

set([], {eb_bean,_Bean}=Wrapper) ->
    Wrapper;

set([{Key,Value}|Props], Wrapper) ->
    NewWrapper = set(Key, Value, Wrapper),
    set(Props, NewWrapper).


set(Key, Value, {eb_bean,Bean}) when is_atom(Key) ->
   NewProps = ?DICT:store(Key, Value, Bean#bean.props),
   {eb_bean,Bean#bean{props=NewProps}}.

