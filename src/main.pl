:- use_module(parse_logs).
:- consult('im_algo.pl').

script_and_dot_from_file(LogFile, DotFile, WriteFile) :-
  read_logs(LogFile, Logs),
  generate_model_with_dot(Logs, DotFile, Graph),
  model_script(Graph, Script),
  setup_call_cleanup(
    open(WriteFile, write, File),
    write_sequence(File, Script),
    close(File)).