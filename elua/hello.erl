%% File    : hello.erl
%% Purpose : Brief demonstration of Luerl basics.
%% Use     $ erlc hello.erl && erl -pa ./ebin -s hello run -s init stop -noshell
%% Or      $ make hello

-module(hello).
-export([run/0]).

run() ->

    S0 = luerl:init(),
    {_R0,       S1} = luerl:dofile("./init.lua", S0),
    {_R1,Chunk1,S2} = luerl:loadfile("./hello.lua", S1),
    {_R2,       S3} = luerl:do(Chunk1, S2),
    {_R3,       S4} = luerl:do(Chunk1, S3),
    {_R4,      _S5} = luerl:do(Chunk1, S4),

    done.
