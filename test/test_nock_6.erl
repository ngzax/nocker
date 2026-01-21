-module(test_nock_6).

-include_lib("eunit/include/eunit.hrl").

%% Opcode 6 is the "Conditional" operator.
%%
%% *[a 6 b c d]       *[a *[[c d] 0 *[[2 3] 0 *[a 4 4 b]]]]
%%

conditional_true_test() ->
  %% [42 [6 [5 [1 42] [0 1]] [1 100] [1 0]]] -> [42 [6 [42] [100] [0]] ->  100
  Nock = nock:parse("[42 [6 [5 [1 42] [0 1]] [1 100] [1 0]]]]"),
  % Nock = nock:parse("[[5 [1 42] [0 1]] [1 100] [1 0]]"),
  % ?assertEqual({ {5, {{1, 42}, {0, 1}}}, {{1, 100}, {1, 0}} }, Nock),

  % ?assertEqual({5, {{1, 42}, {0, 1}}}, noun:at(2, Nock)),
  % ?assertEqual({{1, 100}, {1, 0}}, noun:at(3, Nock)),

  % ?assertEqual({1, 100}, noun:at(6, Nock)),
  % ?assertEqual({1, 0}, noun:at(7, Nock)),

  % ?assertEqual({5, {{1, 42}, {0, 1}}}, noun:at(14, Nock)),
  % ?assertEqual({{1, 100}, {1, 0}}, noun:at(15, Nock)),

  % ?assertEqual({1, 100}, noun:at(30, Nock)),
  % ?assertEqual({1, 0}, noun:at(31, Nock)),

  Result = nock:interpret(Nock),
  ?assertEqual(100, Result).

conditional_false_test() ->
  %% [42 [6 [5 [1 99] [0 1]] [1 100] [1 0]]] -> [42 [6 [99] [100] [0]] ->  0
  Nock = nock:parse("[42 [6 [5 [1 99] [0 1]] [1 100] [1 0]]]]"),
  Result = nock:interpret(Nock),
  ?assertEqual(0, Result).

conditional_not_operator_test() ->
  %% [0 [6 [5 [1 0] [0 1]] [1 1] [1 0]]] -> [0 [6 [0] [1] [0]] -> 1
  Nock = nock:parse("[0 [6 [5 [1 0] [0 1]] [1 1] [1 0]]]"),
  Result = nock:interpret(Nock),
  ?assertEqual(1, Result).
