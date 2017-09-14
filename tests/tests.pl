:- use_module('../src/parsers/parse_script').
:- consult('../src/imd/im_algo.pl').
:- consult('../main.pl').


match_scripts(OutputFile, ResultFile) :-
    read_script(OutputFile, Outputs),
    read_script(ResultFile, Results),
    !,
    Results == Outputs.

% LogFile is the file containing the input logs
% OutputFile is the file where the result of the Imd
% algorithm are written
% ResultFile is the containing the answers. OutputFile
% should match ResultFile
% Example: launch_tests('event_log.txt', 'results.txt', 'event_script.txt')
launch_tests(LogFile, OutputFile, ResultFile) :-
    script_from_file(LogFile, OutputFile),
    match_scripts(OutputFile, ResultFile).

launch_tests(LogFile, OutputFile, ResultFile) :-
    script_from_file(LogFile, OutputFile),
    read_script(OutputFile, Outputs),
    read_script(ResultFile, Results),
    write('Expected: \n'),
    write(Results),
    write('\n'),
    write('Got: \n'),
    write(Outputs).