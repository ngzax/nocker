-module(test_nock_5).
-include_lib("eunit/include/eunit.hrl").

%% Opcode 5 is the "equality" operator. = (wut)
%% *[a 5 b c]   =[*[a b] *[a c]]
%%
%% Opcode 5 implements the idiomatic = tis equality operator,
%% which tests for deep equality between two nouns.
%%
%% Opcode 5 tests whether the products of formulas b and c are 
%% structurally identical. It returns 0 if equal, 1 if not.
%%
%% 1. Evaluate b against the subject to produce noun l.
%% 2. Evaluate c against the subject to produce noun r.
%% 3. Return 0 if l and r are identical nouns, 1 otherwise.
%%
%% =[a a]              0
%% =[a b]              1
%%
%% Like opcode 3, = tis checks by structure, and also by value. 
%% Equality is deep structural comparison: two cells are equal 
%% iff their heads are equal and their tails are equal.
%%

equality_atom_subject_check_true_test() ->
    %% [42 [5 [0 1] [1 42]]] -> [42 42] -> 0 (True)
    Nock = nock:parse("42 [5 [0 1] [1 42]]"),
    Result = nock:interpret(Nock),
    ?assertEqual(true, Result).

equality_atom_subject_check_false_test() ->
    %% [42 [5 [0 1] [1 43]]] -> [42 43] -> 1 (False)
    Nock = nock:parse("42 [5 [0 1] [1 43]]"),
    Result = nock:interpret(Nock),
    ?assertEqual(false, Result).

equality_cell_subject_check_true_test() ->
    %% [[42 43] [5 [0 2] [1 42]]] -> [42 42] -> 0 (True)
    Nock = nock:parse("[42 43] [5 [0 2] [1 42]]"),
    Result = nock:interpret(Nock),
    ?assertEqual(true, Result).

%% This is checking if the 2 atoms in a cell are equal...
equality_cell_subject_check_false_test() ->
    %% [[42 43] [5 [0 2] [0 3]]] -> [42 43] -> 1 (False)
    Nock = nock:parse("[42 43] [5 [0 2] [0 3]]"),
    Result = nock:interpret(Nock),
    ?assertEqual(false, Result).

equality_check_with_computation_test() ->
    %% [42 [5 [0 1] [4 0 1]]] -> [42 43] -> 1 (False)
    Nock = nock:parse("42 [5 [0 1] [4 0 1]]"),
    Result = nock:interpret(Nock),
    ?assertEqual(false, Result).
