-module(eb_bean).


-export([new/1]).
-export([type/1]).
-export([id/1]).
-export([map/2,fold/3]).
-export([get/2,set/2,set/3,export/1,'export/id'/1]).
-export([tainted/1,untaint/1]).

-define(WRAPPER, {eb_bean, Bean}).

-include_lib("erlbean/include/erlbean.hrl").



new(Type) when is_atom(Type) ->
    Dict = ?DICT:new(),
    Dict2 = ?DICT:store(id, undefined, Dict),
    wrap(#bean{type=Type,props=Dict2}).

get(Key, {eb_bean,Bean}) when is_atom(Key) ->
    case ?DICT:find(Key, Bean#bean.props)
        of {ok,Value} -> {ok,Value}
         ; error -> undefined
    end.

set([], Wrapper) ->
    {ok, Wrapper};

set([{Key,Value}|Props], Wrapper) ->
    {ok, NewWrapper} = set(Key, Value, Wrapper),
    set(Props, NewWrapper).


set(Key, Value, ?WRAPPER) when is_atom(Key) ->
   NewProps = ?DICT:store(Key, Value, Bean#bean.props),
   {ok, wrap(Bean#bean{props=NewProps})}.

export(?WRAPPER) ->
    ?DICT:to_list(Bean#bean.props).

'export/id'(?WRAPPER) ->
    Dict2 = ?DICT:erase(id,Bean#bean.props),
    ?DICT:to_list(Dict2).

tainted(?WRAPPER) -> Bean#bean.tainted.

untaint(?WRAPPER) -> wrap(Bean#bean{tainted=false}).

type(?WRAPPER) -> Bean#bean.type.

id(?WRAPPER) ->
    ?DICT:fetch(id, Bean#bean.props).

%% a dict map returns a new dict with values updated with fun, BUT
%% this function returns a list of Keys,Values through the fun, not
%% the bean with updated props
map(Fun, ?WRAPPER) ->
    [Fun(Key, Val) || {Key, Val} <- ?DICT:to_list(Bean#bean.props)].

fold(Fun, Acc, ?WRAPPER) ->
    ?DICT:fold(Fun, Acc, Bean#bean.props).

%%%===================================================================
%%% Internal functions
%%%===================================================================


wrap(#bean{}=Bean) -> ?WRAPPER.
