:- use_module(parse_logs).
:- consult('parse_script.pl').
:- consult('im_algo.pl').

main(FileName) :-
  test1(Script),
  setup_call_cleanup(
    open(FileName, write, File),
    write_sequence(File, Script),
    close(File)).