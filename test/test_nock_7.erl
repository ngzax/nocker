-module(test_nock_7).

-include_lib("eunit/include/eunit.hrl").

%% Opcode 7 is the "Compose" operator.
%%
%% *[a 7 b c]          *[*[a b] c]
%%
%%
%% Opcode 7 provides function composition. It evaluates b against the subject,
%% then uses that result as the subject for evaluating c.
%%
%% This produces a classic “pipe” pattern.
%%
%% 1. Evaluate b against subject to produce an intermediate noun.
%% 2. Evaluate c against that intermediate to produce the final product.
%%
%% When reading it, think of it as “then” or “pipe”.
%% The first formula changes the subject and the second operates on the new subject.
%%

chain_computation_test() ->
  %% [42 [7 [4 0 1] [4 0 1]]] -> 44
  Nock = nock:parse("[42 [7 [4 0 1] [4 0 1]]]"),
  Result = nock:interpret(Nock),
  ?assertEqual(44, Result).

chain_computation_plus_cell_check_test() ->
  %% [42 [7 [4 0 1] [7 [4 0 1] 3 0 1]]] ->  1 (false)
  Nock = nock:parse("[42 [7 [4 0 1] [7 [4 0 1] 3 0 1]]]"),
  Result = nock:interpret(Nock),
  ?assertEqual(false, Result).
 
%% chain_create_cell_from_subject_cell_check_test() ->
%%   %% [42 [7 [[0 1] [0 1]] 3 0 1]] -> 0 (true)
%%   Nock = nock:parse("[42 [7 [[0 1] [0 1]] 3 0 1]]"),
%%   Result = nock:interpret(Nock),
%%   ?assertEqual(1, Result).

