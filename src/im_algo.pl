clearall :-
    retractall(has_causality(_, _)),
    retractall(visited(_)),
    retractall(node(_, _, _)).

:- dynamic (has_causality/2, visited/1, node/3), clearall.

%-----------------------------------------------------------------------------
% Misc functions.
%--

% Check if the element exist
member(X, [X|_]).
member(X, [_|R]) :-
  member(X, R).

% Add an element if not in the list.
unique_add(X, L, L) :-
  member(X, L).
unique_add(X, L, [X|L]).

% Append to list and remove duplicate.
append_list([], [], []).
append_list([], [X|L2], L) :-
  append_list([], L2, R),
  unique_add(X, R, L).
append_list([X|L1], L2, L) :-
  append_list(L1, L2, R),
  unique_add(X, R, L).

% Create alphabet
create_alphabet_sub([], []).
create_alphabet_sub([X|L], [X|A]) :-
  create_alphabet_sub(L, A).
create_alphabet([], []).
create_alphabet([L|R], A) :-
  create_alphabet_sub(L, Ret_L),
  create_alphabet(R, Ret_R),
  append_list(Ret_L, Ret_R, A).

%=============================================================================
% Inductive Mining algorithm
%==

%-----------------------------------------------------------------------------
% Generates a list representing a graph node, its inputs and outputs 
% i.e a list has the form [[x], [a], [b, c]] where x is the node
% and a the input and b, c are the direct children 
%--


state_inputs_sub([], _, []).
state_inputs_sub([X, State|Log], State, [X|Res]) :-
    state_inputs_sub(Log, State, Res).
state_inputs_sub([_|Log], State, Res) :-
    state_inputs_sub(Log, State, Res).

state_inputs([], _, []).
state_inputs([L|Logs], State, T) :-
    member(State, L),
    state_inputs_sub(L, State, L1),
    !,
    state_inputs(Logs, State, L2),
    append_list(L1, L2, T).
state_inputs([_|Logs], State, T) :-
    state_inputs(Logs, State, T).

state_outputs_sub([], _, []).
state_outputs_sub([State, X|Log], State, [X|Res]) :-
    state_outputs_sub(Log, State, Res).
state_outputs_sub([_|Log], State, Res) :-
    state_outputs_sub(Log, State, Res).

state_outputs([], _, []).
state_outputs([L|Logs], State, T) :-
    member(State, L),
    state_outputs_sub(L, State, L1),
    state_outputs(Logs, State, L2),
    append_list(L1, L2, T).
state_outputs([_|Logs], State, T) :-
    state_outputs(Logs, State, T).

generate_graph_sub(_, [], []).
generate_graph_sub(Logs, [S|States], Graph) :-
    state_inputs(Logs, S, In),
    state_outputs(Logs, S, Out),
    generate_graph_sub(Logs, States, Res),
    append_list([[[S], In, Out]], Res, Graph).

generate_graph(Logs, States, Graph) :-
    generate_graph_sub(Logs, States, Graph).

%-----------------------------------------------------------------------------
% Adds causality relations into the database
% i.e for two activites (A, B), A -> B
%--

add_causality_rec(_, []).
add_causality_rec(State, [X|Out]) :- 
    assert(has_causality(State, X)),
    add_causality_rec(State, Out).

add_causality([]).
add_causality([Node|R]) :-
    unpack_node(Node, State, _, Output),
    add_causality_rec(State, Output),
    add_causality(R).

add_nodes([]).
add_nodes([Node|Graph]) :-
    unpack_node(Node, State, In, Out),
    select(Val, State, []),
    assert(node(Val, In, Out)),
    add_nodes(Graph).

%-----------------------------------------------------------------------------
% Adds the graphs' relations into the database
%--

% Retrieves a node and its inputs/ouputs from the graph
unpack_node([S, Ins, Outs], [Node], Ins, Outs) :-
    select(Node, S, []).

create_database(Graph) :-
    add_causality(Graph),
    add_nodes(Graph).

%-----------------------------------------------------------------------------
% Generate singleton trees from the graph 
%--
generate_trees([], []).
generate_trees([Node|Graph], [Val|Res]) :-
    unpack_node(Node, Val, _, _),
    generate_trees(Graph, Res).

%-----------------------------------------------------------------------------
% Set of functions performing a sequential cut.
% First step is to find the connected components.
% Second step is to merge the unreachable pairwise components.
%--

path_rec(A, [X|_]) :-
    path(A, X).
path_rec(A, [_|T]) :-
    path_rec(A, T).
path_rec([Nd|_], Target) :-
    path(Nd, Target).
path_rec([_|Clique], Target) :-
    path_rec(Clique, Target).

path_sub(_, Target, [O|_]) :-
    path([O], Target).
path_sub(A, Target, [_|Out]) :-
    path_sub(A, Target, Out).

path(A, A).
path([Nd|Clique], Target) :-
    path_rec([Nd|Clique], Target).
path(A, [X|Target]) :-
    path_rec(A, [X|Target]).
path(A, Target) :-
    \+ is_list(A),
    \+ is_list(Target),
    \+ visited(A),
    assert(visited(A)),
    node(A, _, Out),
    path_sub(A, Target, Out).

connected_components(C, [], C).
connected_components(A, [B|Graph], Clique) :-
    path(A, B),
    path(B, A),
    append(A, B, C),
    retractall(visited(_)),
    connected_components(C, Graph, Clique).
connected_components(A, [_|Graph], Clique) :-
    connected_components(A, Graph, Clique).

remove_nodes([], L, L).
remove_nodes([C|Clique], G1, G2) :-
    delete(G1, [C], Res),
    remove_nodes(Clique, Res, G2).
remove_nodes([_|Clique], G1, G2) :-
    remove_nodes(Clique, G1, G2).

cut_graph([], []).
cut_graph([A|G1], [Clique|Components]) :-
    connected_components(A, G1, Clique),
    remove_nodes(Clique, G1, G2),
    retractall(visited(_)),
    cut_graph(G2, Components).
    
apply_imd(Graph, NewGraphs) :-
    cut_graph(Graph, NewGraphs).


test1 :-
    Logs=[[a, b, c, f, g, h, i], [a, b, c, g, h, f, i], [a, b, c, h, f, g, i],
          [a, c, b, f, g, h, i], [a, c, b, g, h, f, i], [a, c, b, h, f, g, i],
          [a, d, f, g, h, i], [a, d, e, d, g, h, f, i], [a, d, e, d, e, d, h, f, g, i]],
    create_alphabet(Logs, States),
    generate_graph(Logs, States, Graph),
    create_database(Graph),
    generate_trees(Graph, Trees),
    apply_imd(Trees, _).

test2 :-
    Logs=[[a, b, c, d], [a, b, d, c, b]], 
    create_alphabet(Logs, States),
    generate_graph(Logs, States, Graph),
    create_database(Graph),
    generate_trees(Graph, Trees),
    apply_imd(Trees, _).