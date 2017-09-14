:- use_module(graph_utils).

clearall :-
    retractall(visited(_)),
    retractall(node(_, _, _, _)),
    retractall(neg_node(_, _, _)),
    retractall(loop_node(_, _, _)),
    retractall(current_graph(_)),
    retractall(graph_inputs(_, _)),
    retractall(graph_outputs(_, _)).

:- dynamic (visited/1, node/4, neg_node/3, loop_node/3,
            current_graph/1, graph_start/2, graph_end/2), clearall.

%-----------------------------------------------------------------------------
% Misc functions.
%--

incr(X, NX) :-
    NX is X + 1.

first([X|_], X).

retrieve_id(Id) :-
    current_graph(Id),
    !.

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
create_alphabet([], L, L).
create_alphabet([L|R], Acc, Res) :-
    \+ subset([L], Acc),
    append_list(Acc, [L], AccBis),
    create_alphabet(R, AccBis, Res).
create_alphabet([_|R], Acc, Res) :-
    create_alphabet(R, Acc, Res).

%=============================================================================
% Inductive Mining algorithm
%==

%-----------------------------------------------------------------------------
% Adds a node into the database.
% The node has the form [[x], [a, b], [c, d]] with a,b the inputs
% and c,d the outputs 
%--

add_nodes(_, [], _).
add_nodes(Id, [Clique|Graph], Method) :-
    unpack_node(Clique, State, In, Out),
    select(Val, State, []),
    Pred =.. [Method, Id, Val, In, Out],
    assert(Pred),
    add_nodes(Id, Graph, Method).

create_database(Graph) :-
    graph_id(Graph, Id, G),
    add_nodes(Id, G, node).

%-----------------------------------------------------------------------------
% Generate singleton trees from the graph 
%--

generate_trees([], []).
generate_trees([Clique|Graph], [Val|Res]) :-
    unpack_node(Clique, Val, _, _),
    generate_trees(Graph, Res).

%=============================================================================
% CUT ALGORITHMS
%==

%-----------------------------------------------------------------------------
% Set of functions performing a BASE CUT. 
% This is technically not a cut. 
% Detects graphs with a single activity left, i.e [a]
%--

base_cut(Graph, Base) :-
    flatten(Graph, Flat),
    length(Flat, 1),
    select(Base, Flat, []).

%-----------------------------------------------------------------------------
% Set of functions performing a SEQUENTIAL CUT. 
% First step is to find the connected components.
% Second step is to merge the unreachable pairwise components.
% Fails if there a no changes (No cuts were found)
%--

%---
% Path looks for a path between two graphs 
% The graphs are lists which contains one or many nodes 
% i.e [a, b] and [c, d, e]
%--

%-- STEP 1
% Gets the nodes from the graphs
path_rec(A, [X|_], Caller) :-
    path(A, X, Caller).
path_rec(A, [_|T], Caller) :-
    path_rec(A, T, Caller).
path_rec([Nd|_], Target, Caller) :-
    path(Nd, Target, Caller).
path_rec([_|Clique], Target, Caller) :-
    path_rec(Clique, Target, Caller).

% Calls path on the node's children
path_sub(_, Target, [O|_], Caller) :-
    path([O], Target, Caller).
path_sub(A, Target, [_|Out], Caller) :-
    path_sub(A, Target, Out, Caller).

% Visits a node and marks it.
% Then calls path_sub on its children
path(A, A, _) :-
    assert(visited(A)).
path([Nd|Clique], Target, Caller) :-
    path_rec([Nd|Clique], Target, Caller).
path(A, [X|Target], Caller) :-
    path_rec(A, [X|Target], Caller).
path(A, Target, Caller) :-
    \+ is_list(A),
    \+ is_list(Target),
    \+ visited(A),
    assert(visited(A)),
    retrieve_id(Id),
    call(Caller, Id, A, _, Out),
    path_sub(A, Target, Out, Caller).

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

% The sequential cut is valid if, for all components i and j,
% i != j, if i -> j then j -x-> i
is_sequential_cut_sub(_, []).
is_sequential_cut_sub(G, [H|List]) :-
    directed_path(G, H),
    \+ directed_path(H, G),
    is_sequential_cut_sub(G, List).
is_sequential_cut_sub(G, [H|List]) :-
    directed_path(H, G),
    \+ directed_path(G, H),
    is_sequential_cut_sub(G, List).

% Remove G from the graph otherwise the check is not valid 
% since the condition i != j will not hold
is_sequential_cut([], _).
is_sequential_cut([G|List], Graph) :-
    subtract(Graph, [G], Tmp), 
    is_sequential_cut_sub(G, Tmp),
    is_sequential_cut(List, Graph).

% Finds the ssc (strongly connected components)
% then merges the ssc's which are not connected at all
sequential_cut(Graph, seq, NewGraphs) :-
    sequential_cut_sub(Graph, SubTrees),
    merge_graphs(SubTrees, NewGraphs),
    !,
    \+ length(NewGraphs, 1), % A sequential cut cannot yield a single component
    is_sequential_cut(NewGraphs, NewGraphs).

%-----------------------------------------------------------------------------
% Set of functions performing an EXCLUSIVE CUT. 
% Within a graph of the form [a, b, c, d], finds the sscs so that the result is
% in the form [[a, b], [c, d]]
% Fails if there a no changes (No cuts were found)
%--

% Finds the sscs in the graph
divide_node_sub(_, [], []).
divide_node_sub(Elt, [X|L], [X|Res]) :-
    ssc(Elt, X),
    divide_node_sub(Elt, L, Res).
divide_node_sub(Elt, [_|X], Res) :-
    divide_node_sub(Elt, X, Res).

divide_node([], []).
divide_node([Elt|L1], [Res|NewGraph]) :-
    divide_node_sub(Elt, L1, Tmp),
    append([Elt], Tmp, Res), % Append Elt to head of list to keep the order
    subtract(L1, Res, L2),
    divide_node(L2, NewGraph).

exclusive_cut_sub([], []).
exclusive_cut_sub([G|Graph], NewGraphs) :-
    divide_node(G, G1),
    exclusive_cut_sub(Graph, G2),
    append(G1, G2, NewGraphs).

% Checks if the cut is valid, i.e that the computed sscs
% never reach each other
is_exclusive_cut_sub(_, []).
is_exclusive_cut_sub(G, [H|List]) :-
    not_connected(G, H),
    is_exclusive_cut_sub(G, List).

is_exclusive_cut([], _).
is_exclusive_cut([G|List], Graph) :-
    subtract(Graph, [G], G1), 
    is_exclusive_cut_sub(G, G1),
    is_exclusive_cut(List, Graph).

exclusive_cut(Graph, alt, NewGraphs) :-
    exclusive_cut_sub(Graph, NewGraphs),
    !,
    is_exclusive_cut(NewGraphs, NewGraphs),
    NewGraphs \= Graph. % No changes -> Failure

%-----------------------------------------------------------------------------
% Set of functions performing a CONCURRENT CUT. 
% First negates the graph
% Secondly, computes the concurrent activities by taking the connected components
% The parallel activities are the connected components
% Fails if there a no changes (No cuts were found)
%--

% Returns the full graph from the graph passed as parameter
% i.e for each node, we get the input and output nodes
% For instance, [a] becomes [[a], [], [b, c]] with b and c the outputs
full_graph_sub(_, [], []).
full_graph_sub(Id, [N|Cliques], [[[N], In, Out]|Res]) :-
    node(Id, N, In, Out),
    full_graph_sub(Id, Cliques, Res).

full_graph(_, [], []).
full_graph(Id, [G|Graph], Res) :-
    full_graph_sub(Id, G, L1),
    full_graph(Id, Graph, L2),
    append(L1, L2, Res).

% For each node, switches the inputs and the ouputs
% If the node contains an element x in both the input and ouput sets
% x is removed from both sets
negate_states_sub(In, Out, NewIn, NewOut) :-
    intersection(In, Out, Intersect),
    \+ length(Intersect, 0),
    subtract(In, Intersect, NewIn),
    subtract(Out, Intersect, NewOut).
negate_states_sub(In, Out, In, Out).

negate_states([], []).
negate_states([G|Graph], [[A, NewOut, NewIn]|NegGraph]) :-
    unpack_node(G, A, In, Out),
    negate_states_sub(In, Out, NewIn, NewOut),
    negate_states(Graph, NegGraph).

% Computes the negated graph and saves it into the database
negate_graph(Graph, NegGraph) :-
    retrieve_id(Id),
    full_graph(Id, Graph, Full),
    negate_states(Full, NegGraph),
    add_nodes(Id, NegGraph, neg_node).

% Removes an element from the full graph
% for instance, if A = [a], removes the node [[a], [x], [x]]
% from the graph 
remove_elt(_, [], []).
remove_elt(A, [G|Graph], Res) :-
    unpack_node(G, B, _, _),
    subset(B, A),
    remove_elt(A, Graph, Res).
remove_elt(A, [G|Graph], [G|Res]) :-
    remove_elt(A, Graph, Res).

% Computes the parallel sets in the graph
% The parallel sets are the connected components of negated graph
parallel_components(C, [], C).
parallel_components(A, [G|Graph], Res) :-
    unpack_node(G, B, _, _),
    connected(A, B, neg_node),
    append(A, B, C),
    parallel_components(C, Graph, Res).
parallel_components(A, [_|Graph], Res) :-
    parallel_components(A, Graph, Res).

parallel_cut_sub([], []).
parallel_cut_sub([G|Graph], [C|NewGraphs]) :-
    unpack_node(G, A, _, _),
    parallel_components(A, Graph, C),
    remove_elt(C, Graph, GraphBis),
    parallel_cut_sub(GraphBis, NewGraphs).

% The parallel cut is valid if for each cluster :
% The intersection between the cluster and the start set is not empty
% The intersection between the cluster and the end set is not empty
is_parallel_cut([], _, _).
is_parallel_cut([G|Graph], Start, End) :-
    intersection(Start, G, StartIntersect),
    \+ length(StartIntersect, 0),
    intersection(End, G, EndIntersect),
    \+ length(EndIntersect, 0),
    is_parallel_cut(Graph, Start, End).

% Negates the graph and computes the parallel components
parallel_cut(Graph, par, NewGraphs) :-
    activities('start', Graph, Graph, Start),
    activities('end', Graph, Graph, End),
    negate_graph(Graph, NegGraph),
    parallel_cut_sub(NegGraph, NewGraphs),
    !,
    \+ length(NewGraphs, 1), % A parallel cut cannot yield a single component
    is_parallel_cut(NewGraphs, Start, End),
    retractall(neg_node(_, _, _)),
    NewGraphs \= Graph.

%-----------------------------------------------------------------------------
% Set of functions performing a LOOP CUT. 
% First, the connected components of the graph are computed by removing the
% start and end states. 
% Then, each component is examined to determine whether it goes from
% the start states to the end states (Body component) or the other way
% around (Redo component).
%--

% Removes the loop body from the full_graph's transitions
remove_transitions(_, [], []).
remove_transitions(Body, [G|Graph], [[A, NewIn, NewOut]|Res]) :-
    unpack_node(G, A, In, Out),
    subtract(In, Body, NewIn),
    subtract(Out, Body, NewOut),
    remove_transitions(Body, Graph, Res).

% Removes the loop body from the graph
remove_body(_, [], []).
remove_body(Body, [G1|Graph], [G2|Res]) :-
    subtract(G1, Body, G2),
    remove_body(Body, Graph, Res).

% Puts a graph in the form [[a], [b,c]] to the form
% [[a], [b], [c]]
deconstruct_graph([], []).
deconstruct_graph([G|Graph], [A|Res]) :-
    unpack_node(G, A, _, _),
    deconstruct_graph(Graph, Res).

% Regroups the loop activities into clusters
% Those clusters will join the body or will be part of the redo.
loop_components(CC, [], CC).
loop_components(A, [G|Graph], Res) :-
    connected(A, G, loop_node),
    append(A, G, CC),
    loop_components(CC, Graph, Res).
loop_components(A, [_|Graph], Res) :-
    loop_components(A, Graph, Res).

% loop_cc computes the loop's connected components
% by getting the start & end activities of the loop
% and removing them from the graph

% Computes the loops connected components
% from the graph without the start & end states
loop_cc_sub([], []).
loop_cc_sub([A|Graph], [Res|CC]) :-
    loop_components(A, Graph, Res),
    remove_nodes(Res, Graph, G1),
    loop_cc_sub(G1, CC).

loop_cc(Graph, Start, End, CC) :-
    retrieve_id(Id),
    activities('start', Graph, Graph, Start), % Gets start activities
    activities('end', Graph, Graph, End),
    append_list(Start, End, TempBody),
    remove_body(TempBody, Graph, Res), % Removes Start & End activities
    full_graph(Id, Res, FullGraph),
    remove_transitions(TempBody, FullGraph, ReducedGraph),
    add_nodes(Id, ReducedGraph, loop_node),
    deconstruct_graph(ReducedGraph, Deconstructed),
    loop_cc_sub(Deconstructed, CC).

% Returns the path just inserted in the database
retrieve_path([X|Res]) :-
    retract(visited(X)),
    retrieve_path(Res).
retrieve_path([]).

% C is part of the loop body if it reaches the start states
% by going through at least one end state
is_body(C, Start, End) :-
    retractall(visited(_)),
    path(C, Start, node),
    retrieve_path(Path),
    !,
    intersection(Path, End, Res),
    \+ length(Res, 0).

% C is part of the loop redo if it reaches the end states
% by going through at least one start state
is_redo(C, Start, End) :-
    retractall(visited(_)),
    path(C, End, node),
    retrieve_path(Path),
    !,
    intersection(Path, Start, Res),
    \+ length(Res, 0).

% Tells whether an activity is the body part of the loop
% or the redo part
body_redo([], _, _, [], []).
body_redo([C|Component], Start, End, [C|Body], Redo) :-
    is_body(C, Start, End),
    body_redo(Component, Start, End, Body, Redo).
body_redo([C|Component], Start, End, Body, [C|Redo]) :-
    is_redo(C, Start, End),
    body_redo(Component, Start, End, Body, Redo).

% Takes the loop's connected components and adds them either
% to the Body cluster or to the Redo cluster
reachability(Components, Start, End, [Body, Redo]) :-
    body_redo(Components, Start, End, TempBody, TempRedo),
    flatten(TempBody, FlatBody),
    flatten(TempRedo, Redo),
    \+ length(Redo, 0), % If no redo, we must use the second clause
    append_list(Start, FlatBody, B1),
    append_list(B1, End, Body).

reachability(Components, Start, End, [Body]) :-
    body_redo(Components, Start, End, TempBody, _),
    flatten(TempBody, FlatBody),
    append_list(Start, FlatBody, B1),
    append_list(B1, End, Body).

loop_cut_sub(Graph, NewGraphs) :-
    loop_cc(Graph, Start, End, Components),
    !,
    reachability(Components, Start, End, NewGraphs).

loop_cut(Graph, loop, NewGraphs) :-
    loop_cut_sub(Graph, NewGraphs),
    !,
    NewGraphs \= Graph.

%=============================================================================
% imd is the main_algorithm 
% first, it looks for a base case for the graph, if not found, 
% it tries different cut methods
%==

% Find cut tries to split the graph according to the four operators
find_cut(Graph, Operator, NewGraphs) :-
    exclusive_cut(Graph, Operator, NewGraphs).
find_cut(Graph, Operator, NewGraphs) :-
    sequential_cut(Graph, Operator, NewGraphs).
find_cut(Graph, Operator, NewGraphs) :-
    parallel_cut(Graph, Operator, NewGraphs).
find_cut(Graph, Operator, NewGraphs) :-
    loop_cut(Graph, Operator, NewGraphs).

% Splits transforms sets from the cuts into graphs,
% i.e  [b, c, d, e] becomes [[b, c, d, e]]
split([], []).
split([C|Cuts], [[C]|Res]) :-
    split(Cuts, Res).

% Calls imd on all the subgraphs generated by the cuts
imd_sub([], []).
imd_sub([G|Graph], [SubGraph|Res]) :-
    imd(G, SubGraph),
    imd_sub(Graph, Res).

imd(Graph, Script) :-
    base_cut(Graph, Script).
imd(Graph, Script) :-
    find_cut(Graph, Operator, Cuts),
    split(Cuts, SubGraphs),
    imd_sub(SubGraphs, NewGraphs),
    Script =.. [Operator, NewGraphs].
imd(Graph, Script) :- % No cuts -> Returns a loop (as in the paper) 
    flatten(Graph, FlatGraph),
    Script =.. [loop, FlatGraph].

%=============================================================================
% Header method of the IMD algorithm
% If the resulting Script are generated from more than one graph, an 'alt' 
% operator is added since those graphs are different possibilities
% Otherwise the script is returned

%-----------------------------------------------------------------------------
% Retrieves a graph with the same start as the logs
%--
existing_graph_sub(Start, [G|_], G) :-
    graph_id(G, _, Graph),
    first(Graph, First),
    unpack_node(First, A, _, _),
    select(Val, A, []),
    Val == Start.
existing_graph_sub(Start, [_|List], Res) :-
    existing_graph_sub(Start, List, Res).

existing_graph([Start|_], List, Graph) :-
    existing_graph_sub(Start, List, Graph).

%-----------------------------------------------------------------------------
% Generates the graphs from the logs.
% If the graph with the same start already exists, complete it
% Otherwise, create a new graph representing a new possibility
%-- 
add_start_activity([X|_], Id) :-
    graph_start(Id, Start),
    unique_add(X, Start, NewStart),
    retract(graph_start(Id, Start)),
    assert(graph_start(Id, NewStart)).
add_start_activity([X|_], Id) :-
    assert(graph_start(Id, [X])).

add_end_activity(Logs, Id) :-
    last(Logs, X),
    graph_end(Id, Start),
    unique_add(X, Start, NewStart),
    retract(graph_end(Id, Start)),
    assert(graph_end(Id, NewStart)).
add_end_activity([X|_], Id) :-
    assert(graph_end(Id, [X])).

generate_graph(Log, States, Id, Id, L1, [Graph|L2]) :-
    existing_graph(Log, L1, G),
    graph_id(G, GId, _),
    add_start_activity(Log, GId),
    add_end_activity(Log, GId),
    delete(L1, G, L2),
    complete_graph(Log, States, G, Graph).

generate_graph(L, States, Id, NewId, List, [Graph|List]) :-
    create_graph(L, States, G1),
    append([Id], G1, G2),
    assert(graph_start(Id, [])),
    assert(graph_end(Id, [])),
    add_start_activity(L, Id),
    add_end_activity(L, Id),
    incr(Id, NewId),
    create_database(G2),
    order_graph(Id, L, Graph).

generate_model_sub([], Id, Id, L, L).
generate_model_sub([L|Logs], OldId, NewId, List, Res) :-
    create_alphabet(L, [], States),
    generate_graph(L, States, OldId, Id, List, NewList),
    generate_model_sub(Logs, Id, NewId, NewList, Res).

generate_model([], _, _, []).
generate_model([Line|Lines], Id, List, [S1|Res]) :-
    generate_model_sub(Line, Id, NewId, List, S1),
    generate_model(Lines, NewId, [], Res).

% Generates the script from the model
scripts_sub(Graph, Script) :-
    graph_id(Graph, Id, G),
    assert(current_graph(Id)),
    generate_trees(G, Trees),
    imd(Trees, Script),
    retractall(current_graph(_)).

scripts([], []).
scripts([G|Graphs], [Res|Script]) :-
    scripts_sub(G, Res),
    scripts(Graphs, Script).

model_script_sub(Graphs, Script) :-
    length(Graphs, 1),
    scripts(Graphs, Res),
    select(Script, Res, []).

model_script_sub(Graphs, Script) :-
    scripts(Graphs, Res),
    Script =.. [alt, Res].

model_script([], []).
model_script([Graph|GraphList], [Res|Scripts]) :-
    model_script_sub(Graph, Res),
    model_script(GraphList, Scripts).

%=============================================================================
% Writes the result of the IM Algorithm into a file 
%--

% Writes a terminal term to the file
write_operator(File, seq, 'Op') :-
    write(File, 'SEQ(').
write_operator(File, alt, 'Op') :-
    write(File, 'ALT(').
write_operator(File, par, 'Op') :-
    write(File, 'PAR(').
write_operator(File, loop, 'Op') :-
    write(File, 'LOOP(').
write_operator(File, Script, 'Term') :-
    Script =.. List,
    length(List, 1), % Terminal value 
    write(File, Script).

% Writes a separator between function arguments
% only if there are other args left
write_args_separator(File, FlatArgs, Type) :-
    Type == 'Term',
    \+ length(FlatArgs, 0),
    write(File, ', ').
write_args_separator(_, _, _).

write_separator(File, Args) :-
    \+ length(Args, 0),
    write(File, ', ').
write_separator(_, _).

% Iterates through function args and writes them to the file
write_args(_, []).
write_args(File, [A|Args]) :-
    write_operator(File, A, Type),
    flatten(Args, FlatArgs),
    write_args_separator(File, FlatArgs, Type),
    write_args(File, FlatArgs).
write_args(File, [A|Args]) :-
    write_sequences_sub(File, A),
    write_separator(File, Args),
    write_args(File, Args).

% Write a sequence into the file
% A sequence contains an operator and an argument list
write_sequences_sub(File, Script) :-
   Script =.. List,
   write_args(File, List), 
   write(File, ')').

write_sequences(_, []).
write_sequences(File, [S|Scripts]) :-
    write_sequences_sub(File, S),
    write(File, '\n'),
    flush_output(File),
    write_sequences(File, Scripts).

test1(Script) :-
    Logs=[[a, b, c, f, g, h, i], [a, b, c, g, h, f, i], [a, b, c, h, f, g, i],
          [a, c, b, f, g, h, i], [a, c, b, g, h, f, i], [a, c, b, h, f, g, i],
          [a, d, f, g, h, i], [a, d, e, d, g, h, f, i], [a, d, e, d, e, d, h, f, g, i]],
    generate_model(Logs, Graph),
    model_script(Graph, Script).