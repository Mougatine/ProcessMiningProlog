:- module(parse_script, [read_script/2]).
:- use_module(library(pio)).

%-----------------------------------------------------------------------------
% Module file containing methods for reading event script files
% and parsing the input
%--

% DCG Grammar
% lines := eos
%       | line lines
%
% line := '\n'
%      | eos
%      | operators line
%
% operators := operator
%           | operator ',' operators
%
% array_exp := exps
%           | exps ',' array_exp
%
% exps := args
%      | operators
%
% args := elt
%      | elt ',' args
%
% elt := [value]
%
% operator := [operator] '(' array_exp ')'


lines([]) --> call(eos), !.
lines([Line|Lines]) --> line(X), { flatten(X, Line) }, lines(Lines).

eos([], []).

line([]) --> ( "\n" ; call(eos) ), !.
line([Log|Ls]) --> operators(Log), line(Ls).

operators(Op) --> operator(Op).
operators([Op, Ops]) --> operator(Op), ", ", operators(Ops).

array_exp(Args) --> exps(Args).
array_exp([X|Args]) --> exps(X), ", ", array_exp(Args).

exps(Terms) --> args(Terms).
exps(Terms) --> operators(Terms).

args([Arg]) --> elt(Arg).
args([X|Args]) --> elt(X), ", " , args(Args).

elt(Arg) --> [X], { atom_codes(Arg, [X]) }.

operator(Term) --> "SEQ", "(", array_exp(Op), ")", { build_term(seq, Op, Term) }.
operator(Term) --> "ALT", "(", array_exp(Op), ")", { build_term(alt, Op, Term) }.
operator(Term) --> "PAR", "(", array_exp(Op), ")", { build_term(par, Op, Term) }.
operator(Term) --> "LOOP", "(", array_exp(Op), ")", { build_term(loop, Op, Term) }.

% build_term creates a term from a functor
% and a list of arguments, or from a composition of functors
build_term(Operation, Args, Term) :-
    flatten(Args, FlatArgs),
    append([Operation], FlatArgs, Res),
    Term =.. Res.

% read_file reads a file content until EOF, and then closes it
read_script(Path, Logs) :-
    phrase_from_file(lines(Logs), Path).