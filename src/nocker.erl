-module(nocker).
-export([
    %% nock functions
    parse/1,
    interpret/1,
    interpret/2,
    nock/1,
    %% noun functions
    at/2,
    from_list/1,
    to_list/1,
    to_string/1,
    is_atom/1,
    is_cell/1
]).

%% Nock functions
parse(String) -> nock:parse(String).
interpret(Expr) -> nock:interpret(Expr).
interpret(Subject, Formula) -> nock:interpret(Subject, Formula).
nock(Input) -> nock:nock(Input).

%% Noun functions
at(Index, Noun) -> noun:at(Index, Noun).
from_list(List) -> noun:from_list(List).
to_list(Noun) -> noun:to_list(Noun).
to_string(Noun) -> noun:to_string(Noun).
is_atom(Noun) -> noun:is_atom(Noun).
is_cell(Noun) -> noun:is_cell(Noun).
