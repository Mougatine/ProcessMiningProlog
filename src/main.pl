:- use_module(parse_logs).
:- consult('im_algo.pl').

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