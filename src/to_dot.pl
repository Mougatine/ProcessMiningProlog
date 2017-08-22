:- consult('src/alpha-algo.pl').

%-----------------------------------------------------------------------------
% Write states from a petri's net into a file.
%--

% Write recursively all the states in the list L to the stream File.
% Recall: the states are represented by the list Yl.
%
% write_states_sub(File, L).
write_states_sub(_, []).
write_states_sub(File, [X|L]) :-
  write(File, '    "'),
  write(File, X),
  write(File, '";\n'),
  write_states_sub(File, L).

% Write in the stream File, the section that define all the states in the
% list L and the shape of a state.
% Recall: the states are represented by the list Yl.
%
% write_states(File, L).
write_states(File, Yl) :-
  write(File, '  subgraph state {\n'),
  write(File, '    node [shape=circle,fixedsize=true,width=0.5,label=""];\n'),
  write(File, '    "initial_state";\n'),
  write_states_sub(File, Yl),
  write(File, '    "final_state";\n'),
  write(File, '  }\n').

%-----------------------------------------------------------------------------
% Write activities from petri's net into a file.
%--

% Write recursively all the activities in the list L to the stream File.
% Recall: the activities are represented by the Alphabet.
%
% write_activites_sub(File, L).
write_activities_sub(_, []).
write_activities_sub(File, [X|L]) :-
  write(File, '    "'),
  write(File, X),
  write(File, '";\n'),
  write_activities_sub(File, L).

% Write in the stream File, the section that define all the activities in the
% list L and the shape of an activity.
% Recall: the activities are represented by the Alphabet.
%
% write_states(File, L).
write_activities(File, Alphabet) :-
  write(File, '  subgraph activities {\n'),
  write(File, '    node [shape=rect,height=0.5,width=0.5];\n'),
  write_activities_sub(File, Alphabet),
  write(File, '  }\n').

%-----------------------------------------------------------------------------
% Write transitions from a petri's net into a file.
%--

% Write recursively transitions that are output of state T (and input of the
% activities in L).
%
% write_transitions_out(File, T, L).
write_transitions_out(_, _, []).
write_transitions_out(File, T, [E|L]) :-
  write(File, '  "'),
  write(File, T),
  write(File, '" -> "'),
  write(File, E),
  write(File, '";\n'),
  write_transitions_out(File, T, L).

% Write recursively transitions that are input of state T (and output of the
% activities in L).
%
% write_transitions_in(File, T, L).
write_transitions_in(_, _, []).
write_transitions_in(File, T, [S|L]) :-
  write(File, '  "'),
  write(File, S),
  write(File, '" -> "'),
  write(File, T),
  write(File, '";\n'),
  write_transitions_in(File, T, L).

% Write all the transition of the petri's net. It take as input the stream to
% write into and the Yl representing the petri's net.
%
% write_transitions(File, Yl).
write_transitions(_, []).
write_transitions(File, [[S,E] | L]) :-
  write_transitions_in(File, [S,E], S),
  write_transitions_out(File, [S,E], E),
  write_transitions(File, L).

% Write all transition from initial state to activities in Ti (initial
% activities).
%
% write_initial_transitions(File, Ti).
write_initial_transitions(_, []).
write_initial_transitions(File, [X|L]) :-
  write(File, '  "initial_state" -> "'),
  write(File, X),
  write(File, '";\n'),
  write_initial_transitions(File, L).

% Write all transition from activities in To (final activities) to final state.
%
% write_final_transitions(File, To).
write_final_transitions(_, []).
write_final_transitions(File, [X|L]) :-
  write(File, '  "'),
  write(File, X),
  write(File, '" -> "final_state";\n'),
  write_final_transitions(File, L).

%-----------------------------------------------------------------------------
% Create a petri's net from logs and write into a file.
%--

% Create a petri's net from the given Logs and write it in File.
write_graph(File, Logs) :-
  clearall,
  create_alphabet(Logs, Alphabet),
  create_Ti(Logs, Ti),
  create_Tl(Logs, Tl),
  create_To(Logs, To),
  create_database(Alphabet, Tl),
  create_Yl(Alphabet, Yl),
  write(File, "Digraph G {\n"),
  write_states(File, Yl),
  write_activities(File, Alphabet),
  write_initial_transitions(File, Ti),
  write_transitions(File, Yl),
  write_final_transitions(File, To),
  write(File, "}\n").

% Open the File at path FileName, create and write petri's net into, then close
% File.
write_dot(Logs, FileName) :-
  setup_call_cleanup(
    open(FileName, write, File),
    write_graph(File, Logs),
    close(File)).

%-----------------------------------------------------------------------------
% TESTS
%--

dot_test1 :-
  Logs=[[a,b,c,d],[a,c,b,d],[a,e,d]],
  FileName='test1.dot',
  write_dot(Logs, FileName).

dot_test2 :-
  Logs=[[a,b,c,d,e,g], [a,b,d,c,e,f,b,c,d,e,g],
       [a,b,c,d,e,f,b,c,d,e,f,b,d,c,e,g]],
  FileName='test2.dot',
  write_dot(Logs, FileName).
