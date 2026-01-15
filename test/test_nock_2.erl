-module(test_nock_2).
-include_lib("eunit/include/eunit.hrl").

%% Opcode 2 is the "evaluate" operator
%%
%% *[a 2 b c] -> *[*[a b] *[a c]]
%%

subject_echo_function_test() ->
    %% [42 [1 [0 1]]] -> 42
    Nock = nock:parse("[42 [0 1]]"),
    Result = nock:interpret(Nock),
    ?assertEqual(42, Result).

constant_function_test() ->
    %% https://urbit.org/blog/nockmas-2025-day-3
    %% *[a 2 b c] -> *[*[a b] *[a c]]
    %% [42 [2 [1 100] [1 [0 1]]]] -> 100
    Nock = nock:parse("[42 [2 [1 100] [1 [0 1]]]]"),
    Result = nock:interpret(Nock),

    %% This is a
    % ?assertEqual(42, nock:subject(Nock)),

    %% This is b
    % ?assertEqual({1, 100}, nock:subject(nock:formula(nock:formula(Nock)))),
    % ?assertEqual(1, nock:opcode(nock:subject(nock:formula(nock:formula(Nock))))),

    %% This is c
    % ?assertEqual({1, {0, 1}}, nock:formula(nock:formula(nock:formula(Nock)))),
    % ?assertEqual(1, nock:opcode(nock:formula(nock:formula(nock:formula(Nock))))),

    ?assertEqual(100, Result).

dynamic_formula_test() ->
    %% https://urbit.org/blog/nockmas-2025-day-3
    %% [[5 4 0 1] [2 [0 2] [0 3]]] -> 6
    Nock = nock:parse("[[5 4 0 1] [2 [0 2] [0 3]]]"),
    Result = nock:interpret(Nock),
    ?assertEqual(6, Result).
