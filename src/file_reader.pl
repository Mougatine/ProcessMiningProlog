:- module(file_reader, [read_file/2]).

% read_file reads a file content until EOF, and then closes it
% the file MUST contain prolog terms
% otherwise read_file fails
read_file(Path, Logs) :-
    open(Path, read, Stream),
    read_logs(Stream, Logs),
    close(Stream),
    write(Logs), nl.

% read_logs reads all the logs until EOF 
read_logs(Stream, []) :-
    at_end_of_stream(Stream).

read_logs(Stream, [Log | Tail]) :-
    \+ at_end_of_stream(Stream),
    read(Stream, Log),
    read_logs(Stream, Tail).