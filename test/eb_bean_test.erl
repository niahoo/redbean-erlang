-module(eb_bean_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlbean/include/erlbean.hrl").


create_test() ->
    ?assertMatch({eb_bean, #bean{type=testb, tainted=true}}, eb_bean:new(testb)).



getset_test() ->
    Bean = eb_bean:new(testb),
    {ok, Bean2} = Bean:set(mykey,<<"My Value">>),
    {ok, Bean2} = Bean:set([{mykey,<<"My Value">>}]),
    {ok, BeanO} = Bean:set([{mykey,<<"My Value">>},{mykey_2,<<"My Value 2">>}]),
    ?assertEqual(Bean2:get(mykey),{ok, <<"My Value">>}).

is_bean_test() ->
    Bean = eb_bean:new(testb),
    ?assert(Bean:is_bean()).

set_make_tainted_test() ->
    Bean = eb:dispense(testb),
    {ok, Bean2} = Bean:set([{aaa,"aaa"},{bbb,"bbb"}]),
    Bean2_ut = Bean2:untaint(),
    ?assertEqual(false, Bean2_ut:tainted()),
    {ok, Bean3} = Bean2_ut:set([{aaa,"zzz"},{xxx,"yyy"}]),
    ?assertEqual(true, Bean3:tainted()).

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


fold_test() ->
    Bean = eb_bean:new(testb),
    {ok, Bean2} = Bean:set([{'id',1},{b,2},{c,3},{d,4}]),
    ?assertEqual(24, Bean2:fold(fun(_K,V,Acc) -> Acc*V end, 1)).

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
