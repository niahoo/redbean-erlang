-module(eb_bean_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlbean/include/erlbean.hrl").


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

meta_test_() ->
    Bean = eb:dispense(bean),
    {ok, Bean2} = Bean:set_meta({aaa, bbb}, [value1]),
    {ok, Bean3} = Bean2:set_meta([
            {{ooo, zzz}, valueX}
        ]),
    {ok, Bean4} = Bean3:append_meta({aaa, bbb}, value2),
    {ok, Bean5} = Bean4:append_meta(other, other_value),
    [ ?_assertMatch({ok, [value1,value2]}, Bean5:get_meta({aaa,bbb}))
    , ?_assertMatch({ok, valueX}, Bean5:get_meta({ooo,zzz}))
    , ?_assertMatch(undefined, Bean5:get_meta(nonexistingkey))
    ].


simple_own_test() ->
    Book = eb:dispense(book),
    Chapter = eb:dispense(chapter),
    {ok, Chap2} = Chapter:set(title, "My Title"),
    {ok, Book2} = Book:own(Chap2),
    ?assertMatch({ok, [Chap2]}, Book2:get_meta({own, chapter})).

