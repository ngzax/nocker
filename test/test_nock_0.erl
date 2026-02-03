-module(test_nock_0).
-include_lib("eunit/include/eunit.hrl").

%%
%% Nock 0 is Tree Addressing
%% *[a 0 b] -> /[b a]
%%

slot_1_test() ->
    %% [[50 51] [0 1]]
    Nock = nock:parse("[50 51] [0 1]"),
    Subject = noun:at(2, Nock),
    Opcode = noun:at(2, noun:at(3, Nock)),
    Slot = noun:at(3, noun:at(3, Nock)),

    %% Verify structure
    ?assertEqual([50, 51], noun:to_list(Subject)),
    ?assertEqual(0, Opcode),
    ?assertEqual(1, Slot),

    %% Interpret: should return the whole subject
    Result = nock:interpret(Nock),
    ?assertEqual({50, 51}, Result),
    ?assertEqual([50, 51], noun:to_list(Result)).

slot_2_test() ->
    %% [[50 51] [0 2]]
    Nock = nock:parse("[50 51] [0 2]"),
    Opcode = noun:at(2, noun:at(3, Nock)),
    Slot = noun:at(3, noun:at(3, Nock)),

    ?assertEqual(0, Opcode),
    ?assertEqual(2, Slot),

    %% Interpret: should return the head of subject (50)
    Result = nock:interpret(Nock),
    ?assertEqual(50, Result).

bad_slot_test() ->
    %% [[50 51] [0 8]] - slot 8 doesn't exist in [50 51]
    Nock = nock:parse("[50 51] [0 8]"),
    Opcode = noun:at(2, noun:at(3, Nock)),
    Slot = noun:at(3, noun:at(3, Nock)),

    ?assertEqual(0, Opcode),
    ?assertEqual(8, Slot),

    %% Should crash on invalid slot
    ?assertThrow({error, _}, nock:interpret(Nock)).

cell_slot_fails_test() ->
    %% [[50 51] [0 [0 1]]] - slot must be an atom
    Nock = nock:parse("[50 51] [0 [0 1]]"),
    %% Should crash when slot is a cell
    ?assertThrow({error, slot_must_be_atom}, nock:interpret(Nock)).

right_hand_cell_omission_test() ->
    %% [1 [2 3]] can be written equivalently as [1 2 3]
    Nock1 = nock:parse("1 [2 3]"),
    Nock2 = nock:parse("1 2 3"),
    ?assertEqual(Nock1, Nock2).

large_subject_tail_test() ->
    % Interpret: should return the tail of subject ([51 52])
    Nock = nock:parse("[[41 42 [43 44] [45 46] [47 48] [49 50]] [51 52]] [0 3]"),
    Result = nock:interpret(Nock),
    ?assertEqual([51, 52], noun:to_list(Result)).

