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
                Aborted = eb:proc([abort]),
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
  ]}.


startapp() ->
    ok = application:start(erlbean),
    eb:setup(epgsql,?PGTESTCONF),
    started.


stopapp(_) ->
    error_logger:tty(false),
    ok = application:stop(erlbean),
    error_logger:tty(true).
