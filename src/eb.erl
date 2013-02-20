-module(eb).

-include_lib("erlbean/include/erlbean.hrl").

-export([db1/0,db2/0,ship/0]).



-export([setup/2,setup/3,setup/4]).
-export([proc/1]).
-export([dispense/1]).
-export([load/2,find/1,find/2,find/3]).
-export([store/1]).
-export([exec/1,exec/2]).
-export([get_col/1,get_col/2,get_rows/1,get_rows/2]).
-export([get_eb_db/0]).

%% TEST --------------------------------------------------------------

-define(TESTCONF, [{user,"test"},{password,"test"},{host,"localhost"},{opts,[{database,"test"}]}]).


db1() -> eb:setup(epgsql,?TESTCONF).
db2() -> eb:setup(epgsql,other_eb_db, ?TESTCONF).

ship() ->
    db1(),
    Ship = eb:dispense(ship),
    {ok, Ship2} = Ship:set([{name, "My Hobocab"},{itemtype_id, 34}]),
    {ok, Ship3} = eb:store(Ship2),
    Ship3.


%% Setup -------------------------------------------------------------

setup(Adapter, Conf) ->
    setup(Adapter, default_eb_db, Conf, fluid).

setup(Adapter, Conf, frozen) ->
    setup(Adapter, default_eb_db, Conf, fluid);

setup(Adapter, Name, Conf) ->
    setup(Adapter, Name, Conf, fluid).

setup(Adapter, Name, Conf, FMode) when is_atom(Adapter) ->
    erlbean_sup:start_toolkit(Adapter, Name, Conf, FMode).

%% Queries -----------------------------------------------------------
%% Works directly with the adapter

%% exec relays the query directly to the adapter, the return format is
%% not normalized
exec(Query) -> exec(Query,[]).

exec(Query, Bindings) ->
    eb_adapter:exec(dba(), Query, Bindings).

get_col(Query) -> get_col(Query,[]).

get_col(Query, Bindings) ->
    eb_db:get_col(Query, Bindings).

get_rows(Query) -> get_rows(Query, []).

get_rows(Query, Bindings) ->
    eb_db:get_rows(Query, Bindings).


%% Beans -------------------------------------------------------------

dispense(Type) -> eb_bean:new(Type).


store(Wrapper) -> eb_db:store(Wrapper).

load(Type, ID) -> eb_db:load(Type, ID).

find(Type) -> find(Type, [], []).
find(Type, AddSQL) -> find(Type, AddSQL, []).
find(Type, AddSQL, Bindings) -> eb_db:find(Type, AddSQL, Bindings).

%% Procs -------------------------------------------------------------

proc(Procedures) -> eb_proc:do(Procedures).


%% regarde dans gproc le toolkit enregistré. Si pas de toolkit on
%% catch une exception enregistré on  renvoie le toolkit par defaut :
%% default_eb_db. Pour éviter une exception les fois suivantes, on
%% l'enregistre dans gproc
get_eb_db() -> eb_db:get_eb_db().


%% ===================================================================
%% INTERNAL
%% ===================================================================

db() -> eb_db:get_eb_db().
dba() -> eb_db:get_adapter(db()).
