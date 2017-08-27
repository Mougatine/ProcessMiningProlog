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

%=============================================================================
% CUT ALGORITHMS
%==

%-----------------------------------------------------------------------------
% Set of functions performing a SEQUENTIAL CUT. 
% First step is to find the connected components.
% Second step is to merge the unreachable pairwise components.
%--

% Returns true if A and B are in the same strongly connected component
ssc(A, B) :-
    retractall(visited(_)),
    path(A, B),
    !,
    retractall(visited(_)),
    path(B, A).

% Returns true if A and B are not connected
not_connected(A, B) :-
    retractall(visited(_)),
    \+ path(A, B),
    retractall(visited(_)),
    \+ path(B, A).

%---
% Path looks for a path between two graphs 
% The graphs are lists which contains one or many nodes 
% i.e [a, b] and [c, d, e]
%--

%-- STEP 1
% Gets the nodes from the graphs
path_rec(A, [X|_]) :-
    path(A, X).
path_rec(A, [_|T]) :-
    path_rec(A, T).
path_rec([Nd|_], Target) :-
    path(Nd, Target).
path_rec([_|Clique], Target) :-
    path_rec(Clique, Target).

% Calls path on the node's children
path_sub(_, Target, [O|_]) :-
    path([O], Target).
path_sub(A, Target, [_|Out]) :-
    path_sub(A, Target, Out).

% Visits a node and marks it.
% Then calls path_sub on its children
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

% Returns the strongly connected component
% containing the node A
connected_components(C, [], C).
connected_components(A, [B|Graph], Clique) :-
    ssc(A, B),
    append(A, B, C),
    connected_components(C, Graph, Clique).
connected_components(A, [_|Graph], Clique) :-
    connected_components(A, Graph, Clique).

% Removes the nodes merged into a clique
% from the main graph
remove_nodes(_, [], []).
remove_nodes(Clique, [X1|G1], G2) :-
    intersection(Clique, X1, X2),
    \+ length(X2, 0),
    remove_nodes(Clique, G1, G2).
remove_nodes(Clique, [X|G1], [X|G2]) :-
    remove_nodes(Clique, G1, G2).

sequential_cut_sub([], []).
sequential_cut_sub([A|G1], [Clique|Components]) :-
    connected_components(A, G1, Clique),
    remove_nodes(Clique, G1, G2),
    sequential_cut_sub(G2, Components).

%-- STEP 2
% Merges unreachable pairwise components
merge_graphs_sub(C, [], C).
merge_graphs_sub(Tree, [X|Graph], NewTree) :-
    not_connected(Tree, X),
    append(Tree, X, TempTree),
    merge_graphs_sub(TempTree, Graph, NewTree).
merge_graphs_sub(Tree, [_|Graph], NewTree) :-
    merge_graphs_sub(Tree, Graph, NewTree).

merge_graphs([], []).
merge_graphs([T|SubTrees], [G|NewGraphs]) :-
    merge_graphs_sub(T, SubTrees, G),
    remove_nodes(G, SubTrees, Res),
    merge_graphs(Res, NewGraphs).

% Finds the ssc (strongly connected components)
% then merges the ssc's which are not connected at all
sequential_cut(Graph, seq, NewGraphs) :-
    sequential_cut_sub(Graph, SubTrees),
    merge_graphs(SubTrees, NewGraphs).

%-----------------------------------------------------------------------------
% Set of functions performing an EXCLUSIVE CUT. 
% Within a graph of the form [a, b, c, d], finds the sscs so that the result is
% in the form [[a, b], [c, d]]
%--
remove_elt(_, [], []).
remove_elt(L1, [Elt|L2], Res) :-
    member(Elt, L1),
    remove_elt(L1, L2, Res).
remove_elt(L1, [Elt|L2], [Elt|Res]) :-
    remove_elt(L1, L2, Res).

divide_node_sub(X, [], [X]).
divide_node_sub(Elt, [X|L], [X|Res]) :-
    ssc(Elt, X),
    divide_node_sub(Elt, L, Res).
divide_node_sub(Elt, [_|X], Res) :-
    divide_node_sub(Elt, X, Res).

divide_node([], []).
divide_node([Elt|L1], NewGraph) :-
    divide_node_sub(Elt, L1, R1),
    remove_elt(R1, L1, L2)
    divide_node(L2, R2),
    append(R1, R2, NewGraph).

exclusive_cut_sub([], []).
exclusive_cut_sub([G|Graph], [G1|NewGraphs]) :-
    divide_node(G, G1),
    exclusive_cut_sub(Graph, NewGraphs).

exclusive_cut(Graph, alt, NewGraphs) :-
    exclusive_cut_sub(Graph, NewGraphs).

%-----------------------------------------------------------------------------
% Set of functions performing a BASE CUT. 
% Within a graph of the form [a, b, c, d], finds the sscs so that the result is
% in the form [[a, b], [c, d]]
%--
    
apply_imd(Graph, NewGraphs) :-
    sequential_cut(Graph, NewGraphs).


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