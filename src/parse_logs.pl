:- module(parse_logs, [read_logs/2]).
:- use_module(library(pio)).

%-----------------------------------------------------------------------------
% Module file containing methods for reading event script files
% and parsing the input
%--

lines([]) --> call(eos), !.
lines([Line|Lines]) --> line(Line), lines(Lines).

eos([], []).

line([]) --> ( "\n" ; call(eos) ), !.
line([Log|Ls]) --> "[", log(Log), "]", line(Ls).

log(Log) --> event(Log).
log([X,Log]) --> event(X), ", ", log(Log).

event(Log) --> "<", op_list(X), ">", { flatten(X, Log) }.

op_list([Log]) --> elt(Log).
op_list([X, Log]) --> elt(X), ", ", op_list(Log).

elt(Arg) --> [X], { atom_codes(Arg, [X]) }.

% read_file reads a file content until EOF, and then closes it
read_logs(Path, Logs) :-
    phrase_from_file(lines(Logs), Path).