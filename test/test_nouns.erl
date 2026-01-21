-module(test_nouns).
-include_lib("eunit/include/eunit.hrl").

%%
%% Noun Tests
%%

from_list_test() ->
    ?assertEqual(42, noun:from_list([42])),
    ?assertEqual({1, 2}, noun:from_list([1, 2])),
    ?assertEqual({1, {2, 3}}, noun:from_list([1, 2, 3])).

to_list_test() ->
    ?assertEqual([42], noun:to_list(42)),
    ?assertEqual([1, 2], noun:to_list({1, 2})),
    ?assertEqual([[1, 2], 3], noun:to_list({{1, 2}, 3})).

at_test() ->
    Cell = {50, 51},
    ?assertEqual({50, 51}, noun:at(1, Cell)),
    ?assertEqual(50, noun:at(2, Cell)),
    ?assertEqual(51, noun:at(3, Cell)).

at_beyond_3_test() ->
    % [[4 5] [6 14 15]]
    Noun = {{4, 5}, {6, {14, 15}}},
    ?assertEqual({{4, 5}, {6, {14, 15}}}, noun:at(1, Noun)),
    ?assertEqual({4, 5}, noun:at(2, Noun)),
    ?assertEqual({6, {14, 15}}, noun:at(3, Noun)),
    ?assertEqual(14, noun:at(14, Noun)),
    ?assertEqual(15, noun:at(15, Noun)).

at_interpret_test() ->
    Noun = noun:from_list([[5, [1, 42], [0, 1]], [1, 100], [0, 1]]),
    ?assertEqual({5, {{1, 42}, {0, 1}}}, noun:at(2, Noun)),
    ?assertEqual(5, noun:at(4, Noun)),
    ?assertEqual({{1, 42}, {0, 1}}, noun:at(5, Noun)),
    ?assertEqual({{1, 100}, {0, 1}}, noun:at(3, Noun)),
    ?assertEqual({1, 100}, noun:at(6, Noun)),
    ?assertEqual({0, 1}, noun:at(7, Noun)).

increment_test() ->
    ?assertEqual(51, noun:increment(50)).

is_atom_test() ->
    ?assert(noun:is_atom(42)),
    ?assertNot(noun:is_atom({1, 2})).

is_cell_test() ->
    ?assert(noun:is_cell({1, 2})),
    ?assertNot(noun:is_cell(42)).
