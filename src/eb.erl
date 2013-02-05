-module(eb).

-extends(erlbean_facade).

-export([t/0]).
-export([db1/0,db2/0]).

-define(TESTCONF, [{user,"test"},{password,"test"},{host,"localhost"},{opts,[{database,"test"}]}]).

t() ->
    catch db1(),
    eb_adapter_epgsql:exec(eb:get_toolkit(), "select 3 + 5").

db1() -> eb:setup(epgsql,?TESTCONF).
db2() -> eb:setup(epgsql,other_toolkit, ?TESTCONF).

