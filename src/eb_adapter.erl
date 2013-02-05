-module(eb_adapter).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{store,2},
     {exec, 2},
     {exec, 3}
    ];

behaviour_info(_Other) ->
    undefined.
