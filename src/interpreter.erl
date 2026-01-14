-module(interpreter).
-export([opcode/3]).

%% Nock 0: Tree addressing
%% *[a 0 b] -> /[b a]
%%
%% Opcode 0 implements the idiomatic / "fas" slot operator, which retrieves a noun at a specified address within the subject.

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
%%
%% Opcode 1 is the simplest conceivable Nock operation: it simply returns its argument b, ignoring its subject a entirely.
%%
%% It is used to store data for use in later computations.
%%
%% This can either be conventional data as atoms (including byte arrays) and structured data in a cell format;
%%   or it can be Nock code itself, which can be retrieved and executed later using opcode 9.

opcode(1, _Subject, Formula) ->
    slot(Formula);

%% Nock 2: Evaluate
%% *[a 2 b c] -> *[*[a b] *[a c]]
%%
%% Opcode 2 implements the * tar evaluate operator, which dynamically computes a new subject and formula,
%%   then evaluates the formula against the subject.
%%
opcode(2, Subject, Formula) ->
    %% Evaluate the subject against position 2 (the subject) of the formula to get a new Subject
    B = nock:subject(nock:formula(Formula)),
    NewSubject = opcode(nock:opcode(B), Subject, B),

    %% Evaluate the subject against position 3 (the formula) of the formula to get a new Formula
    C = nock:formula(nock:formula(Formula)),
    NewFormula = opcode(nock:opcode(C), Subject, C),

    Base = noun:from_list(noun:to_list(NewSubject) ++ noun:to_list(NewFormula)),
    nock:interpret(Base);

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
