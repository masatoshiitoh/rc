%% File    : hello.erl
%% Purpose : Brief demonstration of Luerl basics.
%% Use     $ erlc hello.erl && erl -pa ./ebin -s hello run -s init stop -noshell
%% Or      $ make hello

-module(hello).
-export([run/0]).

run() ->

    % execute a file
    {ok, F, State1} = luerl:loadfile("./hello.lua"),

    % separately parse, then execute
    State0 = luerl:init(),
    {ok, Chunk, State2} = luerl:do("a=0", State1),
    {_Ret, State3} = luerl:do(Chunk, State2),
    {_Ret, State4} = luerl:do(Chunk, State3),
    {_Ret, _State5} = luerl:do(Chunk, State4),

    done.
