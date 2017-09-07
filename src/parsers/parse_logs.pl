:- module(parse_logs, [read_logs/2]).
:- use_module(library(pio)).

%-----------------------------------------------------------------------------
% Module file containing methods for reading event script files
% and parsing the input
%--

% DCG Grammar
% line := '[' logs ']'
%      | '[' logs ']' ',' '\n' line
%
% logs := event
%      | event ',' logs 
%
% event := '<' op_list '>'
%       | '<' op_list '>' '^'
%
% op_list := elt
%         | elt ',' op_list
%
% elt := [arg]

line([Logs]) --> "[", logs(Logs), "]".
line([X|Logs]) --> "[", logs(X), "],\n", line(Logs). 

logs(Logs) --> event(Logs).
logs(Logs) --> event(X), ", ", logs(Y), { append(X, Y, Logs) }.

event([Logs]) --> "<", op_list(Logs), ">". 
event([Logs]) --> "<", op_list(Elts), ">^", get_digit(Num), { generate_loop(Elts, Num, Logs) }. 

op_list([Logs]) --> elt(Logs).
op_list([X|Logs]) --> elt(X), ", ", op_list(Logs).

elt(Arg) --> [X], { atom_codes(Arg, [X]) }.

get_digit(Digit) --> [AsciiVal], { digit_from_ascii(AsciiVal, Digit) }.

decr(X,NX) :-
    NX is X-1.

digit_from_ascii(AsciiVal, Digit) :-
    char_code(X, AsciiVal),
    atom_number(X, Digit).

generate_loop_sub(_, L, 1, L).
generate_loop_sub(Original, CurrList, Num, Logs) :-
    append(CurrList, Original, Res),
    decr(Num, N),
    generate_loop_sub(Original, Res, N, Logs).

generate_loop(Elts, Num, Logs) :-
    generate_loop_sub(Elts, Elts, Num, Logs),
    !.

% read_file reads a file content until EOF, and then closes it
read_logs(Path, Logs) :-
    phrase_from_file(line(Logs), Path).