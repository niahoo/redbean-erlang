-module(eb_bean).


-export([new/1]).
-export([get/2,set/2,set/3]).
-export([tainted/1,type/1]).

-define(WRAPPER, {eb_bean, Bean}).

-include_lib("erlbean/include/erlbean.hrl").



new(Type) when is_atom(Type) ->
    wrap(#bean{type=Type}).

get(Key, {eb_bean,Bean}) when is_atom(Key) ->
    case ?DICT:find(Key, Bean#bean.props)
        of {ok,Value} -> {ok,Value}
         ; error -> undefined
    end.

set([], ?WRAPPER=Wrapper) ->
    Wrapper;

set([{Key,Value}|Props], Wrapper) ->
    {ok, NewWrapper} = set(Key, Value, Wrapper),
    set(Props, NewWrapper).


set(Key, Value, ?WRAPPER) when is_atom(Key) ->
   NewProps = ?DICT:store(Key, Value, Bean#bean.props),
   {ok, wrap(Bean#bean{props=NewProps})}.

tainted(?WRAPPER) -> Bean#bean.tainted.

type(?WRAPPER) -> Bean#bean.type.

%%%===================================================================
%%% Internal functions
%%%===================================================================


wrap(#bean{}=Bean) -> ?WRAPPER.
