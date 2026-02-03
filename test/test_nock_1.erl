-module(test_nock_1).
-include_lib("eunit/include/eunit.hrl").

%%
%% Nock 1 is the Constant operator
%%
%% *[a 1 b] -> b
%%

echo_atom_test() ->
    %% [[20 30] [1 67]] -> 67
    Nock = nock:parse("[20 30] [1 67]"),
    Result = nock:interpret(Nock),
    ?assertEqual(67, Result).

echo_cell_test() ->
    %% [[20 30] [1 [2 587]]] -> [2 587]
    Nock = nock:parse("[20 30] [1 [2 587]]"),
    Result = nock:interpret(Nock),
    ?assertEqual([2, 587], noun:to_list(Result)).

constant_100_test() ->
    %% [42 [1 100]] -> 100
    Nock = nock:parse("42 [1 100]"),
    Result = nock:interpret(Nock),
    ?assertEqual(100, Result).
