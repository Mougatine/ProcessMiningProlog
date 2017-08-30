:- use_module(parse_logs).
:- consult('im_algo.pl').

script_from_file(LogFile, WriteFile) :-
  read_logs(LogFile, Logs),
  generate_model(Logs, Graph),
  model_script(Graph, Script),
  setup_call_cleanup(
    open(WriteFile, write, File),
    write_sequence(File, Script),
    close(File)).