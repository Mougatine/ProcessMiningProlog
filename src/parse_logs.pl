:- module(parse_logs, [read_logs/2]).
:- use_module(library(pio)).

%-----------------------------------------------------------------------------
% Module file containing methods for reading event script files
% and parsing the input
%--

line(Logs) --> "[", logs(Logs), "]".

logs(Logs) --> event(Logs).
logs(Logs) --> event(X), ", ", logs(Y), { append(X, Y, Logs)}.

event([Logs]) --> "<", op_list(Logs), ">". 

op_list([Logs]) --> elt(Logs).
op_list([X|Logs]) --> elt(X), ", ", op_list(Logs).

elt(Arg) --> [X], { atom_codes(Arg, [X]) }.

% read_file reads a file content until EOF, and then closes it
read_logs(Path, Logs) :-
    phrase_from_file(line(Logs), Path).