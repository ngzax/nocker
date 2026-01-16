-module(test_nock_5).
-include_lib("eunit/include/eunit.hrl").

%% Opcode 5 is the "equality" operator. = (wut)
%%
%% *[a 3 b] -> ?*[a b]
%%

equality_atom_subject_check_true_test() ->
    %% [42 [5 [0 1] [1 42]]] -> [42 42] -> 0 (True)
    Nock = nock:parse("[42 [5 [0 1] [1 42]]]"),
    Result = nock:interpret(Nock),
    ?assertEqual(true, Result).

equality_atom_subject_check_false_test() ->
    %% [42 [5 [0 1] [1 43]]] -> [42 43] -> 1 (False)
    Nock = nock:parse("[42 [5 [0 1] [1 43]]]"),
    Result = nock:interpret(Nock),
    ?assertEqual(false, Result).

equality_cell_subject_check_true_test() ->
    %% [[42 43] [5 [0 2] [1 42]]] -> [42 42] -> 0 (True)
    Nock = nock:parse("[[42 43] [5 [0 2] [1 42]]]"),
    Result = nock:interpret(Nock),
    ?assertEqual(true, Result).

%% This is checking if the 2 atoms in a cell are equal...
equality_cell_subject_check_false_test() ->
    %% [[42 43] [5 [0 2] [0 3]]] -> [42 43] -> 1 (False)
    Nock = nock:parse("[[42 43] [5 [0 2] [0 3]]]"),
    Result = nock:interpret(Nock),
    ?assertEqual(false, Result).

equality_check_with_computation_test() ->
    %% [42 [5 [0 1] [4 0 1]]] -> [42 43] -> 1 (False)
    Nock = nock:parse("[42 [5 [0 1] [4 0 1]]]"),
    Result = nock:interpret(Nock),
    ?assertEqual(false, Result).
