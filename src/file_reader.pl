:- module(file_reader, [read_file/2]).
:- use_module(library(pio)).

%-----------------------------------------------------------------------------
% Module file containing methods for reading log files
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
% exp := args
%     | operators
%
% elt := [value]
%
% args := elt
%      | elt ',' args


lines([]) --> call(eos), !.
lines([Line|Lines]) --> line(X), { flatten(X, Line) }, lines(Lines).

eos([], []).

line([]) --> ( "\n" ; call(eos) ), !.
line([Log|Ls]) --> operators(Log), line(Ls).

operators(Op) --> operator(Op).
operators([Op, Ops]) --> operator(Op), ", ", operators(Ops).

exp(Terms) --> args(Terms).
exp(Terms) --> operators(Terms).

elt(Arg) --> [Arg].
args([Arg]) --> elt(Arg).
args([X|Args]) --> elt(X), ", " , args(Args).

operator(Term) --> "seq", "(", exp(Op), ")", { build_term(seq, Op, Term) }.
operator(Term) --> "alt", "(", exp(Op), ")", { build_term(alt, Op, Term) }.
operator(Term) --> "par", "(", exp(Op), ")", { build_term(par, Op, Term) }.
operator(Term) --> "loop", "(", exp(Op), ")", { build_term(loop, Op, Term) }.

% build_args convert a list of chars represented by their ascii value
% to a list of intergers
build_args([], []).
build_args([H|Args], [Num|Res]) :-
    atom_codes(Num, [H]),
    build_args(Args, Res).

% build_term creates a term from a functor
% and a list of arguments, or from a composition of functors
build_term(Operation, Args, Term) :-
    build_args(Args, NumArgs),
    append([Operation], NumArgs, Res),
    Term =.. Res.

build_term(Operation, Args, Term) :-
    append([Operation], [Args], Res),
    Term =.. Res.

% read_file reads a file content until EOF, and then closes it
read_file(Path, Logs) :-
    phrase_from_file(lines(Logs), Path).