-module(test_nock_4).
-include_lib("eunit/include/eunit.hrl").

%%
%% Nock 4 is Increment
%%
%% *[a 4 b] -> +*[a b]
%%
increment_test() ->
    %% [50 [4 0 1]] -> 51
    %% Subject is 50, formula is [4 0 1]
    %% *[50 4 0 1] -> +*[50 0 1] -> +50 -> 51
    Nock = nock:parse("[50 [4 0 1]]"),
    Result = nock:interpret(Nock),
    ?assertEqual(51, Result).

recursive_increment_test() ->
    %% [50 [4 4 0 1]] -> 52
    %% Subject is 50, formula is [4 [4 0 1]]
    %% *[50 [4 4 0 1]] -> +*[50 [4 0 1]] -> ++*[50 [0 1]] -> ++/[1 50] -> ++(50) -> +(51) -> 52
    Nock = nock:parse("[50 [4 4 0 1]]"),
    Result = nock:interpret(Nock),
    ?assertEqual(52, Result).

recursive_increment_alternate_syntax_test() ->
    Nock = nock:parse("[50 [4 [4 0 1]]]"),
    Result = nock:interpret(Nock),
    ?assertEqual(52, Result).

increment_with_cell_subject_test() ->
    Nock = nock:parse("[[100 150] [4 4 0 3]]"),
    Result = nock:interpret(Nock),
    ?assertEqual(152, Result).

increment_ignoring_the_subject_test() ->
    Nock = nock:parse("[50 [4 1 98]]"),
    Result = nock:interpret(Nock),
    ?assertEqual(99, Result).

increment_failure_test() ->
    %% Can not increment a cell...
    Nock = nock:parse("[50 [4 1 [0 2]]]"),
    ?assertThrow({error, cannot_increment_cell}, nock:interpret(Nock)).

% increment_with_cell_as_subject_test() ->
    %% Can not increment a cell...
    % Nock = nock:parse("[[19 20] [[0 1] [1 76] [4 4 0 3]]]"),
    % ?assertEqual([[19, 20], 76, 22], nock:interpret(Nock)).
