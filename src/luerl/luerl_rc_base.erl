-module(luerl_rc_base).

-export ([install/1]).

-import(luerl_lib, [lua_error/2,badarg_error/3]).   %Shorten this

install(St) ->
    luerl_emul:alloc_table(table(), St).

table() ->
    [{<<"bar">>,{function,fun bar/2}},
	 {<<"get_all">>, {function, fun get_all/2}},
     {<<"foo">>,{function,fun foo/2}}].

get_all(A,St) ->
{[0], St}.

bar(A,St) -> {[0], St}.
foo(A,St) -> {[0], St}.
