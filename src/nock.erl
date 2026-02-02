-module(nock).
-export([interpret/1, interpret/2, nock/1, parse/1, tokenize/1]).

%% --------------------------------------------------------------------
%% Testing
%%
%% Check if this is a valid Nock expression
%% --------------------------------------------------------------------
is_nock({_Subject, {_OpCode, _Arg}}) ->
    true;
is_nock(_) ->
    false.

%% --------------------------------------------------------------------
%% Interpreter
%%
%% Interpret a Nock expression
%% Nock structure: [Subject Formula]
%% --------------------------------------------------------------------

interpret(NockExpr) ->
    case is_nock(NockExpr) of
        false ->
            throw({error, invalid_nock_expression});
        true ->
            Subject = subject(NockExpr),
            Formula = formula(NockExpr),
            Opcode = opcode(Formula),
            interpret(Opcode, Subject, Formula)
    end.

interpret(Subject, Formula) ->
    %% Create a new Nock expression [Subject Arg (the argument at position 3 of the formula)]
    %%   and interpret it
    interpret(noun:reconstruct(Subject, slot(Formula))).

%% Nock 0: Tree addressing
%% *[a 0 b] -> /[b a]
%%
%% Opcode 0 implements the idiomatic / "fas" slot operator, which retrieves a noun at a specified address within the subject.

interpret(0, Subject, Formula) ->
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

interpret(1, _Subject, Formula) ->
    slot(Formula);

%% Nock 2: Evaluate
%% *[a 2 b c] -> *[*[a b] *[a c]]
%%
%% Opcode 2 implements the * tar evaluate operator, which dynamically computes a new subject and formula,
%%   then evaluates the formula against the subject.

interpret(2, Subject, Formula) ->
    %% Evaluate the subject against position 2 (the subject) of the formula to get a new Subject
    NewSubject = interpret(opcode(b(Formula)), Subject, b(Formula)),

    %% Evaluate the subject against position 3 (the formula) of the formula to get a new Formula
    NewFormula = interpret(opcode(c(Formula)), Subject, c(Formula)),

    interpret(noun:reconstruct(NewSubject, NewFormula));

%% Nock 3: Cell Check
%% *[a 3 b] -> ?*[a b]
%%
%% Opcode 3 tests whether the product of formula b is a cell or an atom.
%% It returns 0 (yes) if it's a cell, 1 (no) if it's an atom.

interpret(3, Subject, Formula) ->
    noun:is_cell(interpret(Subject, Formula));

%% Nock 4: Increment
%% *[a 4 b]         +*[a b]
%%
%% Opcode 4 adds 1 to the product of formula b.
%% This is Nock's only arithmetic primitive—all other arithmetic must be built from increment.

interpret(4, Subject, Formula) ->
    Result = interpret(Subject, Formula),
    %% Increment the result if it's an atom
    case noun:is_atom(Result) of
        true ->
            noun:increment(Result);
        false ->
            throw({error, cannot_increment_cell})
    end;

%% Nock 5: Equality Check
%%
interpret(5, Subject, Formula) ->
    LHS = interpret(opcode(b(Formula)), Subject, b(Formula)),
    RHS = interpret(opcode(c(Formula)), Subject, c(Formula)),
    LHS =:= RHS;

%% Nock 6: Conditional
%%
interpret(6, Subject, Formula) ->
    Cond = interpret(opcode(b(Formula)), Subject, b(Formula)),
    % erlang:display(Formula),
    % erlang:display(slot(Formula)),
    Result = if Cond -> interpret({{Subject}, subject(c(Formula))});
                true -> interpret({{Subject}, formula(c(Formula))})
            end,
    Result;

%% Nock 7: Compose
%%
interpret(7, Subject, Formula) ->
    Intermediate = interpret(opcode(b(Formula)), Subject, b(Formula)),
    Result = interpret({Intermediate, c(Formula)}),
    Result;

interpret(Opcode, _Subject, _Formula) ->
    throw({error, {unknown_opcode, Opcode}}).

%% --------------------------------------------------------------------
%% Parser
%%
%% Parse a string representation into a Nock expression
%% Example: "[[50 51] [0 1]]" -> {{50, 51}, {0, 1}}
%% --------------------------------------------------------------------
parse(String) when is_list(String) ->
    %% Tokenize the string
    Tokens = tokenize(String),
    %% Parse tokens into a list structure
    {Term, _} = parse_tokens(Tokens),
    %% Convert to Noun
    noun:from_list(Term).

%% Parse a list until we hit the closing bracket
parse_list([']'|Rest], Acc) ->
    {lists:reverse(Acc), Rest};
parse_list(Tokens, Acc) ->
    {Elem, Remaining} = parse_tokens(Tokens),
    parse_list(Remaining, [Elem|Acc]).

%% Parse a number from the input
parse_number([C|Rest], Acc) when C >= $0, C =< $9 ->
    parse_number(Rest, [C|Acc]);
parse_number(Rest, Acc) ->
    {Acc, Rest}.

%% Parse tokens into nested list structure
parse_tokens(['['|Rest]) ->
    parse_list(Rest, []);
parse_tokens([N|Rest]) when is_integer(N) ->
    {N, Rest}.

%% --------------------------------------------------------------------
%% Tokenizer
%%
%% Tokenize string into brackets and numbers
%% --------------------------------------------------------------------
tokenize(String) ->
    tokenize(String, []).

tokenize([], Acc) ->
    lists:reverse(Acc);
tokenize([$[|Rest], Acc) ->
    tokenize(Rest, ['['|Acc]);
tokenize([$]|Rest], Acc) ->
    tokenize(Rest, [']'|Acc]);
tokenize([$ |Rest], Acc) ->  % skip spaces
    tokenize(Rest, Acc);
tokenize([C|Rest], Acc) when C >= $0, C =< $9 ->
    {Num, Remaining} = parse_number([C|Rest], []),
    tokenize(Remaining, [list_to_integer(lists:reverse(Num))|Acc]).

%% --------------------------------------------------------------------
%% Main interpreter entry point
%% --------------------------------------------------------------------
nock(Input) when is_list(Input) ->
    Nock = parse(Input),
    io:format("Interpreting ~s as Nock...~n", [Input]),
    Result = interpret(Nock),
    io:format("=> ~p~n", [Result]),
    Result.

%% --------------------------------------------------------------------
%% Navigation Shortcuts
%% --------------------------------------------------------------------

b(NockExpr) ->
    subject(formula(NockExpr)).

c(NockExpr) ->
    formula(formula(NockExpr)).

%% Extract formula from Nock expression: /[3 [subject formula]]
formula(NockExpr) ->
    noun:at(3, NockExpr).

%% Extract opcode from formula: /[2 formula]
opcode(Formula) ->
    noun:at(2, Formula).

%% Extract slot/argument from formula: /[3 formula]
slot(Formula) ->
    noun:at(3, Formula).

%% Extract subject from Nock expression: /[2 [subject formula]]
subject(NockExpr) ->
    noun:at(2, NockExpr).
