-module(test_dist_rule).
-include_lib("eunit/include/eunit.hrl").

%%
%% The Nock interpreter is allowed to return nouns, 
%% which are atoms (positive numbers) or cells (pairs of nouns). 
%%
%% What have our functions/opcodes been returning so far?
%% 
%% 0: atoms or cells, depending what's in the memory slot that we yoink
%% 1: atoms or cells, depending on what we quote
%% 4: just atoms
%%
%% But what if my subject was [51 67 89], and I wanted to increment 
%% every value and return that as [52 68 90]? 
%%
%% How can I do that when it's a cell, and 4 only seems able to return atoms?
%% 
%% The answer is something that the Nock docs call the "Distribution Rule" 
%% or "implicit cons" (hello, fellow LISPers!), but that I find easiest to 
%% think of as the "Cell-Maker Rule."
%%
%% *[subject [formula-x formula-y]] => [*[subject formula-x] *[subject formula-y]]
%%

formula_is_cell_starting_with_atom_test() ->
    Nock = nock:parse("[50 [0 1]]"),
    Result = nock:interpret(Nock),
    ?assertEqual(50, Result).

formula_is_atom_fails_test() ->
    %% Should crash when formula is just an atom
    Nock = nock:parse("[50 0]"),
    ?assertThrow({error, invalid_nock_expression}, nock:interpret(Nock)).

formula_is_2_cells_test() ->
    Nock = nock:parse("[50 [[0 1] [1 203]]]"),
    Result = nock:interpret(Nock),
    ?assertEqual("[50 203]", noun:to_string(Result)).

string_representation_test() ->
    Noun = noun:to_string({33}),
    ?assertEqual("33", Noun),

    N = noun:to_string({33, 44}),
    % N = {33, 44},
    ?assertEqual("[33 44]", N).

many_cells_test() ->
    Nock = nock:parse("[50 [[0 1] [1 203] [0 1] [1 19] [1 76]]]"),
    Result = nock:interpret(Nock),
    ?assertEqual("[50 203 50 19 76]", noun:to_string(Result)).

multiple_ops_test() ->
    %% - [19 20]: [0 1], get memory slot 1
    %% - 76: [1 76], return the quoted value 76
    %% - 22: [4 4 0 3], increment twice the value in memory slot 3 (20)
    Nock = nock:parse("[[19 20] [[0 1] [1 76] [4 4 0 3]]]"),
    Result = nock:interpret(Nock),
    ?assertEqual("[[19 20] 76 22]", noun:to_string(Result)).

