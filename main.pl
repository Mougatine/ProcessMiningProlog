#!/usr/bin/env swipl

:- use_module('./src/imd/parse_logs').
:- consult('./src/imd/im_algo.pl').


:- initialization main.

main:-
  current_prolog_flag(argv, Argv),
  parse_argv(Argv).

parse_argv(Argv) :-
  length(Argv, 2),
  nth0(0, Argv, LogFile),
  nth0(1, Argv, WriteFile),
  script_from_file(LogFile, WriteFile).

parse_argv(Argv) :-
  length(Argv, 4),
  nth0(0, Argv, LogFile),
  nth0(1, Argv, WriteFile),
  nth0(3, Argv, DotFile),
  script_and_dot_from_file(LogFile, DotFile, WriteFile).

parse_argv(_) :-
  write('ERROR: Please follow the usage specified in the README').

script_from_file(LogFile, WriteFile) :-
  read_logs(LogFile, Logs),
  generate_model(Logs, 1, [], Graphs),
  model_script(Graphs, Script),
  setup_call_cleanup(
    open(WriteFile, write, File),
    write_sequence(File, Script),
    close(File)).

script_and_dot_from_file(LogFile, DotFile, WriteFile) :-
  read_logs(LogFile, Logs),
  generate_model(Logs, 1, [], Graphs),
  write_dot(Graphs, DotFile),
  model_script(Graphs, Script),
  setup_call_cleanup(
    open(WriteFile, write, File),
    write_sequence(File, Script),
    close(File)).