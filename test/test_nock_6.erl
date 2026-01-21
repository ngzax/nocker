-module(test_nock_6).
-include_lib("eunit/include/eunit.hrl").

%% Opcode 6 is the "Conditional" operator.
%%
%% *[a 6 b c d]       *[a *[[c d] 0 *[[2 3] 0 *[a 4 4 b]]]]
%%

conditional_true_test() ->
    %% [42 [6 [5 [1 42] [0 1]] [1 100] [1 0]]] -> [42 [6 [42] [100] [0]] ->  100
    Nock = nock:parse("[42 [6 [5 [1 42] [0 1]] [1 100] [1 0]]]"),
    ?assertEqual(42, noun:at(2, Nock)).
    % Result = nock:interpret(Nock),
    % ?assertEqual(100, Result).
