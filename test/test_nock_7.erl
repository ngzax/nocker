-module(test_nock_7).

-include_lib("eunit/include/eunit.hrl").

%% Opcode 6 is the "Compose" operator.
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

%% conditional_false_test() ->
%%   %% [42 [6 [5 [1 99] [0 1]] [1 100] [1 0]]] -> [42 [6 [99] [100] [0]] ->  0
%%   Nock = nock:parse("[42 [6 [5 [1 99] [0 1]] [1 100] [1 0]]]]"),
%%   Result = nock:interpret(Nock),
%%   ?assertEqual(0, Result).
%% 
%% conditional_not_operator_test() ->
%%   %% [0 [6 [5 [1 0] [0 1]] [1 1] [1 0]]] -> [0 [6 [0] [1] [0]] -> 1
%%   Nock = nock:parse("[0 [6 [5 [1 0] [0 1]] [1 1] [1 0]]]"),
%%   Result = nock:interpret(Nock),
%%   ?assertEqual(1, Result).
%%
