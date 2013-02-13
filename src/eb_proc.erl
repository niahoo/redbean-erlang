-module(eb_proc).

-export([do/1]).


do(Procedure) ->
    Stack =  [],
    do(Procedure, Stack).

do([], [Return|Stack]) ->
    Return;

%% Functions cannot pop from stack

do([Fun|Steps], Stack) when is_function(Fun) ->
    debug_step(Fun),
    Result = case erlang:fun_info(Fun, arity)
        of {arity, 0} -> Fun()
         ; {arity, 1} -> Fun(hd(Stack))
         ; {arity, 2} ->
            %% A fun/2 will receive the head of the stack as first
            %% argument, and the tail as second
            [H|T] = Stack,
            Fun(H,T)
         ; {arity, X} -> throw({error, {bad_proc_arith, X}})
    end,
    do(Steps, [Result|Stack]);

do([Step|Steps], Stack) ->
    debug_step(Step),
    do(Steps, [eval(Step, Stack)|Stack]).


%% Placeholder steps -------------------------------------------------

%% Whe put the last item in stack in place of a '_' atom in the
%% command

eval({X,'_'}, [Head|Tail]=Stack) -> eval({X,Head}, Stack);
eval({'_',Y}, [Head|Tail]=Stack) -> eval({Head,Y}, Stack);

%% Same with 3-tuples

eval({X,Y,'_'}, [Head|Tail]=Stack) -> eval({X,Y,Head}, Stack);
eval({X,'_',Z}, [Head|Tail]=Stack) -> eval({X,Head,Z}, Stack);
eval({'_',Y,Z}, [Head|Tail]=Stack) -> eval({Head,Y,Z}, Stack);

%% Normal steps ------------------------------------------------------

eval(abort, _Stack) ->
    aborted;

eval({dispense, Type}, _Stack) ->
    eb_bean:new(Type);

eval({set, Key, Value}, [ABean|Stack]) ->
    Bean = unok(ABean),
    {ok, Bean2} = eb_bean:set(Key, Value, Bean),
    {ok, Bean2};

eval({set, KeyValues}, [ABean|Stack]) when is_list(KeyValues) ->
    Bean = unok(ABean),
    {ok, _Bean2} = eb_bean:set(KeyValues, Bean);

eval({load, Type, ID}, Stack) ->
    eb_db:load(Type, ID);

eval({get, Key}, [ABean|Stack]) ->
    Bean = unok(ABean),
    %% we do not ckeck if {ok, Val} because value can be undefined and that is ok
    eb_bean:get(Key, Bean);

eval(store, [ABean|Stack]) ->
    Bean = unok(ABean),
    {ok, _Bean2} = eb_db:store(Bean);

eval(Step, _Stack) -> throw({error, {unknown_proc, Step}}).


%%% @doc Utility for "untagging" {ok, X} returns
unok({ok, Value}) -> Value;
unok(Value) -> Value.

debug_step(F) when is_function(F) -> log(erlang:fun_to_list(F));
debug_step(S) -> log(S).


log(X) -> error_logger:info_msg("Step : ~p~n~n",[X]).