-module(eb_utils).

-export([typeof/1,to_binary/1]).

to_binary(V) when is_atom(V) -> to_binary(atom_to_list(V)) ;
to_binary(V) when is_list(V) -> list_to_binary(V) ;
to_binary(V) when is_binary(V) -> V.

typeof(X) when is_integer(X)   -> integer;
typeof(X) when is_float(X)     -> float;
typeof(X) when is_list(X)      -> list;
typeof(X) when is_tuple(X)     -> tuple;
typeof(X) when is_bitstring(X) -> bitstring;  % will fail before e12
typeof(X) when is_binary(X)    -> binary;
typeof(X) when is_boolean(X)   -> boolean;
typeof(X) when is_function(X)  -> function;
typeof(X) when is_pid(X)       -> pid;
typeof(X) when is_port(X)      -> port;
typeof(X) when is_reference(X) -> reference;
typeof(X) when is_atom(X)      -> atom;
typeof(_X)                     -> unknown.
