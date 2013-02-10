-module(eb_bean_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlbean/include/erlbean.hrl").


create_test() ->
    ?assertMatch({eb_bean, #bean{type=testb, tainted=true}}, eb_bean:new(testb)).



getset_test() ->
    Bean = eb_bean:new(testb),
    {ok, Bean2} = Bean:set(mykey,<<"My Value">>),
    ?assertEqual(Bean2:get(mykey),{ok, <<"My Value">>}).

gettype_test() ->
    Bean = eb_bean:new(testb),
    ?assertEqual(testb,Bean:type()).

gettainted_test() ->
    Bean = eb_bean:new(testb),
    ?assertEqual(true,Bean:tainted()).

get_id_undefined_test() ->
    Bean = eb_bean:new(testb),
    ?assertMatch({ok, undefined},Bean:get(id)).

map_test() ->
    Bean = eb_bean:new(testb),
    {ok, Bean2} = Bean:set(a, "A"),
    {ok, Bean3} = Bean2:set(b, "B"),
    ?assertMatch([a, b, id],lists:sort(Bean3:map(fun(Key,_Val) -> Key end))).

get_props_test() ->
    Bean = eb_bean:new(testb),
    {ok, Bean2} = Bean:set(a, "A"),
    {ok, Bean3} = Bean2:set(b, "B"),
    ?assertMatch([{a,"A"}, {b, "B"}, {id, undefined}],lists:sort(Bean3:export())),
    ?assertMatch([{a,"A"}, {b, "B"}],lists:sort(Bean3:'export/id'())).

id_shortcut_test() ->
    Bean = eb_bean:new(testb),
    {ok, Bean2} = Bean:set(id,abcdef),
    ?assertMatch(undefined, Bean:id()),
    ?assertMatch(abcdef, Bean2:id()).
