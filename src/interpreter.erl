-module(interpreter).
-export([opcode/3]).

%% Interpret based on opcode
%% Nock 0: Tree addressing
%% *[a 0 b] -> /[b a]
opcode(0, Subject, Formula) ->
    Slot = slot(Formula),
    case noun:is_atom(Slot) of
        false ->
            throw({error, slot_must_be_atom});
        true ->
            noun:at(Slot, Subject)
    end;

%% Nock 1: Constant
%% *[a 1 b] -> b
opcode(1, _Subject, Formula) ->
    slot(Formula);

%% Nock 4: Increment
%% *[a 4 b] -> +*[a b]
opcode(4, Subject, Formula) ->
    %% Get the argument at position 3 of the formula
    Arg = noun:at(3, Formula),
    %% Create a new Nock expression [Subject Arg] and interpret it
    Base = noun:from_list([noun:to_list(Subject), noun:to_list(Arg)]),
    Result = nock:interpret(Base),
    %% Increment the result if it's an atom
    case noun:is_atom(Result) of
        true ->
            noun:increment(Result);
        false ->
            throw({error, cannot_increment_cell})
    end;

opcode(Opcode, _Subject, _Formula) ->
    throw({error, {unknown_opcode, Opcode}}).

%% Extract slot/argument from formula: /[3 formula]
slot(Formula) ->
    noun:at(3, Formula).
