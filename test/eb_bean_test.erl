-module(eb_bean_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlbean/include/erlbean.hrl").


create_test() ->
    ?assertMatch({eb_bean, #bean{type=testb, tainted=true}}, eb_bean:new(testb)).



getset_test() ->
    Bean = eb_bean:new(testb),
    Bean2 = Bean:set(mykey,<<"My Value">>),
    ?assertEqual(Bean2:get(mykey),{ok, <<"My Value">>}).


