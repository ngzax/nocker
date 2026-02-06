-module(test_nock_8).
-include_lib("eunit/include/eunit.hrl").

%% Opcode 8 is the "Extend" operator.
%%
%% *[a 8 b c]          *[[*[a b] a] c]
%%
%% Opcode 8 pins a new value to the head of the subject,
%% then evaluate the body. This is how Nock implements 
%% variable binding—the new value becomes accessible at address 2.
%%
%% 1. Evaluate b against subject to produce a new value.
%% 2. Construct [new-value subject] as the extended subject.
%% 3. Evaluate c against the extended subject.
%%
%% After opcode 8, addresses shift:
%% - Address 2 → the new pinned value
%% - Address 3 → the original subject
%% - Address 6 → what was address 2
%% - Address 7 → what was address 3
%%

bind_local_variable_test() ->
  %% [42 [8 [1 41] [[0 2] [0 3]]] -> [[41 42] [[0 2] [0 3]]] -> [41 42] 
  Nock = nock:parse("42 [8 [1 41] [[0 2] [0 3]]]"),
  Result = noun:to_string(nock:interpret(Nock)),
  ?assertEqual("[41 42]", Result).

bind_local_variable_cell_test() ->
  %% [42 [8 [1 41] [3 [0 1]]]] -> [[41 42] [3 [0 1]]] -> 0 (true, it is a cell)
  Nock = nock:parse("42 [8 [1 41] [3 [0 1]]]"),
  ?assertEqual("true", noun:to_string(nock:interpret(Nock))),
  ?assertEqual(true, nock:interpret(Nock)).

