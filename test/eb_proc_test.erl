-module(eb_proc_test).

-compile([nowarn_unused_function]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlbean/include/erlbean.hrl").
-include_lib("erlbean/include/testcfg.hrl").


interface_test_() ->
  {inorder,
      [{"Test of utilities",
            setup, local, fun startapp/0, fun stopapp/1,
            fun(started) ->
                Aborted = eb:proc([
                    {load, mytype ,999},
                    fun({ok, Bean}) -> Bean:id()
                      ;({not_found, _}) -> abort
                    end,
                    {'NON EXISTING COMMAND'}
                    ]),
                [?_assertEqual(aborted,Aborted)]
            end
        }
       ,{"Test of dispensing Beans",
            setup, local, fun startapp/0, fun stopapp/1,
            fun(started) ->
                MyBean = eb:proc([{dispense, mytype}]),
                [?_assert(eb_bean:is_bean(MyBean))]
            end
        }
       ,{"Test of setting values",
            setup, local, fun startapp/0, fun stopapp/1,
            fun(started) ->
                {ok, MyBean} = eb:proc([
                        {dispense, mytype},
                        {set, name, "robert"},
                        {set, [{age, 23}, {eyes, "blue"}]}
                    ]),
                [?_assertMatch({ok, "robert"}, MyBean:get(name))
                ,?_assertMatch({ok, 23}, MyBean:get(age))
                ,?_assertMatch({ok, "blue"}, MyBean:get(eyes))
                ]
            end
        }
       ,{"Test of loading Beans, simple fun and getting values",
            setup, local, fun startapp/0, fun stopapp/1,
            fun(started) ->
                MyBean = eb:dispense(mytype),
                {ok, MyBean1} = MyBean:set([{name, "johnny"}, {age, 99}]),
                {ok, MyBean2} = eb:store(MyBean1),
                ID = MyBean2:id(),
                {ok, MaybeBean} = eb:proc([{load, mytype, ID}]),
                [?_assert(eb_bean:is_bean(MaybeBean))
                ,?_assertEqual(100, eb:proc([
                        {load, mytype, ID},
                        fun({ok, Bean}) -> {ok, Age} = Bean:get(age), Age + 1 end
                    ]))
                ,?_assertMatch(99, eb:proc([
                        {load, mytype, ID},
                        {get, name},
                        fun({ok, <<"johnny">>}, [{ok, Bean}|_]) ->
                            {ok, Age} = Bean:get(age),
                            Age
                        end
                    ]))
                ]
            end
        }
       ,{"Test of storing Beans, retrieving values, and using placeholders",
            setup, local, fun startapp/0, fun stopapp/1,
            fun(started) ->
                {ok, Bean} = eb:proc([
                        {dispense, mytype},
                        {set, hair, <<"blond">>},
                        store,
                        fun ({ok, Bean}) -> Bean:id() end,
                        {load, mytype, '_'}
                    ]),
                [?_assert(eb_bean:is_bean(Bean))
                ,?_assertMatch({ok, <<"blond">>}, Bean:get(hair))
                ]
            end
        }
       ,{"Test (un)tainting beans",
            setup, local, fun startapp/0, fun stopapp/1,
            fun(started) ->
                {ok, Untainted} = eb:proc([
                        {dispense, mytype},
                        untaint
                    ]),
                {ok, Tainted} = eb:proc([
                        {dispense, mytype},
                        untaint,
                        taint
                    ]),
                [ ?_assertEqual(false, Untainted:tainted())
                , ?_assertEqual(true, Tainted:tainted())
                ]
            end
        }
  ]}.


 % othertest() ->
 %    AgeIsPair = fun({ok, Bean}) ->
 %        {ok, Age} = Bean:get(age),
 %        Age rem 2 =:= 0
 %   end,
 %    eb:proc([
 %        {load, mytype, 23}
 %      , {owned, mysub}
 %      , {each, '_', {set, updated, erlang:now()}},
 %      , {each, '_', store},
 %      , fun(Beans) ->
 %            {_Pairs, Impairs} = lists:partition(AgeIsPair, Beans),
 %            Impairs
 %        end
 %      , {each, '_', trash}
 %    ]).


startapp() ->
    application:start(erlbean),
    eb:setup(epgsql,?PGTESTCONF),
    started.


stopapp(_) ->
    error_logger:tty(false),
    application:stop(erlbean),
    error_logger:tty(true).
