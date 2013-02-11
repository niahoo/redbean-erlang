-module(eb).

-include_lib("erlbean/include/erlbean.hrl").

-export([t/0]).
-export([db1/0,db2/0]).



-export([setup/2,setup/3,setup/4]).
-export([dispense/1]).
-export([load/2]).
-export([store/1]).
-export([get_eb_db/0]).



-define(TESTCONF, [{user,"test"},{password,"test"},{host,"localhost"},{opts,[{database,"test"}]}]).

t() ->
    catch db1(),
    eb_adapter_epgsql:exec(eb:get_eb_db(), "select 3 + 5").

db1() -> eb:setup(epgsql,?TESTCONF).
db2() -> eb:setup(epgsql,other_eb_db, ?TESTCONF).




%% Setup -------------------------------------------------------------

setup(Adapter, Conf) ->
    setup(Adapter, default_eb_db, Conf, fluid).

setup(Adapter, Conf, frozen) ->
    setup(Adapter, default_eb_db, Conf, fluid);

setup(Adapter, Name, Conf) ->
    setup(Adapter, Name, Conf, fluid).

setup(Adapter, Name, Conf, FMode) when is_atom(Adapter) ->
    erlbean_sup:start_toolkit(Adapter, Name, Conf, FMode).


dispense(Type) -> eb_bean:new(Type).


store(Wrapper) -> eb_db:store(Wrapper).

load(Type, ID) -> eb_db:load(Type, ID).

%% regarde dans gproc le toolkit enregistré. Si pas de toolkit on
%% catch une exception enregistré on  renvoie le toolkit par defaut :
%% default_eb_db. Pour éviter une exception les fois suivantes, on
%% l'enregistre dans gproc
get_eb_db() -> eb_db:get_eb_db().


%% ===================================================================
%% INTERNAL
%% ===================================================================

