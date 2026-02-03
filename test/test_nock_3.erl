-module(test_nock_3).
-include_lib("eunit/include/eunit.hrl").

%% Opcode 3 is the "cell check" operator. ? (wut)
%%
%% *[a 3 b] -> ?*[a b]
%%

cell_check_cell_true_test() ->
    %% [42 [3 [1 [0 1]]]] -> [0, 1] -> 0 (True)
    Nock = nock:parse("42 [3 [1 [0 1]]]"),
    Result = nock:interpret(Nock),
    ?assertEqual(true, Result).

cell_check_cell_false_test() ->
    %% [42 [3 [1 1]]] -> 1 -> 1 (False)
    Nock = nock:parse("42 [3 [1 1]]"),
    Result = nock:interpret(Nock),
    ?assertEqual(false, Result).
