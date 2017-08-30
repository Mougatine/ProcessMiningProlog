:- module(graph_utils, [unpack_node/4, state_inputs/3, state_outputs/3, generate_graph/3,
                        activities/4, order_graph/2, ssc/2, not_connected/2, connected/3,
                        directed_path/2]).

%=============================================================================
% Module file containing methods for Graph manipulation
%-----------------------------------------------------------------------------

%-----------------------------------------------------------------------------
% Retrieves a node and its inputs/ouputs from the graph
%--

unpack_node([S, Ins, Outs], [Clique], Ins, Outs) :-
    select(Clique, S, []).

%-----------------------------------------------------------------------------
% Graph reachability methods 
%--

% Returns true if A and B are in the same strongly connected component
ssc(A, B) :-
    retractall(visited(_)),
    path(A, B, node),
    !,
    retractall(visited(_)),
    path(B, A, node).

% Returns true if A and B are not connected
not_connected(A, B) :-
    retractall(visited(_)),
    \+ path(A, B, node),
    retractall(visited(_)),
    \+ path(B, A, node).

% Returns true if there's at least one connection between A and B
connected(A, B, Caller) :-
    retractall(visited(_)),
    path(A, B, Caller).
connected(A, B, Caller) :-
    retractall(visited(_)),
    path(B, A, Caller).

% True if there's a path between A and B
directed_path(A, B) :-
    retractall(visited(_)),
    path(A, B, node).

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
% Gets a graph's start and end activities
%--

activities_sub(_, _, [], []).
activities_sub('start', Graph, [Elt|Clique], [Elt|Res]) :-
    node(Elt, In, _),
    (\+ subset(In, Graph); length(In, 0)),
    activities_sub('start', Graph, Clique, Res).
activities_sub('end', Graph, [Elt|Clique], [Elt|Res]) :-
    node(Elt, _, Out),
    (\+ subset(Out, Graph); length(Out, 0)),
    activities_sub('end', Graph, Clique, Res).
activities_sub(Type, Graph, [_|Clique], Res) :-
    activities_sub(Type, Graph, Clique, Res).

activities(_, _, [], []).
activities(Type, Base, [G|Graph], Activities) :-
    flatten(Base, FlatBase),
    activities_sub(Type, FlatBase, G, L1),
    activities(Type, FlatBase, Graph, L2),
    append(L1, L2, Activities).

%-----------------------------------------------------------------------------
% Orders the graph using a DFS 
%--

get_start([], []).
get_start([G|Graph], [Activity|Start]) :-
    unpack_node(G, A, In, _),
    select(Activity, A, []),
    length(In, 0),
    get_start(Graph, Start).
get_start([_|Graph], Start) :-
    get_start(Graph, Start).

dfs([], []).
dfs([N|Queue], [N|Res]) :-
    unpack_node(N, Val, _, Out),
    select(X, Val, []),
    \+ visited(X),
    assert(visited(X)),
    full_graph_sub(Out, OutGraph),
    append_list(Queue, OutGraph, NewQueue),
    dfs(NewQueue, Res).
dfs([_|Queue], Res) :-
    dfs(Queue, Res).

order_graph(Graph, OrderedGraph) :-
    get_start(Graph, Root),
    select(A, Root, []),
    node(A, In, Out),
    dfs([[Root, In, Out]], OrderedGraph),
    retractall(visited(_)).