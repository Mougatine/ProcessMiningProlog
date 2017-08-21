:- module(file_reader, [read_file/2]).
:- use_module(library(pio)).

%-----------------------------------------------------------------------------
% Module file containing methods for reading log files.
%--

% DCG Grammar

lines([]) --> call(eos), !.
lines([Line|Lines]) --> line(Line), lines(Lines).

eos([], []).

line([]) --> ( "\n" ; call(eos) ), !.
line([Log|Ls]) --> operators(Log), line(Ls).

operators([]) --> call(eos), !.
operators([Op|Ops]) --> operator(Op), ", ", operators(Ops).

operator(seq) --> "seq".
operator(alt) --> "alt".


% read_file reads a file content until EOF, and then closes it
% the file MUST contain prolog terms
% otherwise read_file fails
read_file(Path, Logs) :-
    phrase_from_file(lines(Ls), Path),
    write(Ls), nl.