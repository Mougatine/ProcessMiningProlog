:- module(graph_utils, [unpack_node/4, state_inputs/3, state_outputs/3, create_graph/3,
                        activities/4, order_graph/3, ssc/2, not_connected/2, connected/3,
                        directed_path/2, write_dot/2, graph_id/3, complete_graph/4]).

%=============================================================================
% Module file containing methods for Graph manipulation
%-----------------------------------------------------------------------------

%-----------------------------------------------------------------------------
% Retrieves a node and its inputs/ouputs from the graph
%--

unpack_node([S, Ins, Outs], [Clique], Ins, Outs) :-
    select(Clique, S, []).

%-----------------------------------------------------------------------------
% Removes the id from the graph
%--
graph_id([Id|G], Id, G).

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

state_inputs([], _, []).
state_inputs([X, State|Log], State, Res) :-
    state_inputs(Log, State, Tmp),
    unique_add(X, Tmp, Res).
state_inputs([_|Log], State, Res) :-
    state_inputs(Log, State, Res).

state_outputs([], _, []).
state_outputs([State, X|Log], State, Res) :-
    state_outputs(Log, State, Tmp),
    unique_add(X, Tmp, Res).
state_outputs([_|Log], State, Res) :-
    state_outputs(Log, State, Res).

create_graph_sub(_, [], []).
create_graph_sub(Log, [S|States], Graph) :-
    state_inputs(Log, S, In),
    state_outputs(Log, S, Out),
    create_graph_sub(Log, States, Res),
    append_list([[[S], In, Out]], Res, Graph).

create_graph(Log, States, Graph) :-
    create_graph_sub(Log, States, Graph).

%-----------------------------------------------------------------------------
% Completes a Graph with new logs 
%--
update_node(Id, Nd, In, Out, [A, NewIn, NewOut]) :-
    unpack_node(Nd, A, OldIn, OldOut),
    select(Val, A, []),
    append_list(OldIn, In, NewIn),
    append_list(OldOut, Out, NewOut),
    retract(node(Id, Val, OldIn, OldOut)),
    assert(node(Id, Val, NewIn, NewOut)).

replace_node(_, _, [], []).
replace_node(Nd, NewNode, [Nd|Graph], [NewNode|NewGraph]) :-
    replace_node(Nd, NewNode, Graph, NewGraph).
replace_node(Nd, NewNode, [X|Graph], [X|NewGraph]) :-
    replace_node(Nd, NewNode, Graph, NewGraph).

insert_node(_, _, [], []).
insert_node(Inputs, Node, [G|Graph], [G, Node|NewGraph]) :-
    unpack_node(G, A, _, _),
    subset(A, Inputs),
    insert_node(Inputs, Node, Graph, NewGraph).
insert_node(Inputs, Node, [G|Graph], [G|NewGraph]) :-
    insert_node(Inputs, Node, Graph, NewGraph).

complete_graph_sub(_, _, [], L, L).
complete_graph_sub(Id, Log, [S|States], Graph, Res) :-
    state_inputs(Log, S, In),
    state_outputs(Log, S, Out),
    full_graph_sub(Id, [S], Node),
    first(Node, Nd),
    update_node(Id, Nd, In, Out, NewNode),
    replace_node(Nd, NewNode, Graph, NewGraph),
    complete_graph_sub(Id, Log, States, NewGraph, Res).
complete_graph_sub(Id, Log, [S|States], Graph, Res) :-
    state_inputs(Log, S, In),
    state_outputs(Log, S, Out),
    insert_node(In, [[S], In, Out], Graph, NewGraph),
    assert(node(Id, S, In, Out)),
    complete_graph_sub(Id, Log, States, NewGraph, Res).

complete_graph(Log, States, Graph, NewGraph) :-
    graph_id(Graph, Id, G),
    complete_graph_sub(Id, Log, States, G, Res),
    append([Id], Res, NewGraph).

%-----------------------------------------------------------------------------
% Gets a graph's start and end activities
%--

activities_sub(_, _, [], []).
activities_sub('start', Graph, [Elt|Clique], [Elt|Res]) :-
    retrieve_id(Id),
    node(Id, Elt, In, _),
    (\+ subset(In, Graph); length(In, 0)),
    activities_sub('start', Graph, Clique, Res).
activities_sub('end', Graph, [Elt|Clique], [Elt|Res]) :-
    retrieve_id(Id),
    node(Id, Elt, _, Out),
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

get_start([X|_], X).

dfs(_, [], []).
dfs(Id, [N|Queue], [N|Res]) :-
    unpack_node(N, Val, _, Out),
    select(X, Val, []),
    \+ visited(X),
    assert(visited(X)),
    full_graph_sub(Id, Out, OutGraph),
    append_list(Queue, OutGraph, NewQueue),
    dfs(Id, NewQueue, Res).
dfs(Id, [_|Queue], Res) :-
    dfs(Id, Queue, Res).

order_graph(Id, Logs, OrderedGraph) :-
    get_start(Logs, Root),
    node(Id, Root, In, Out),
    dfs(Id, [[[Root], In, Out]], Res),
    append([Id], Res, OrderedGraph),
    retractall(visited(_)).

%-----------------------------------------------------------------------------
% Writes the graph into a dot file 
%--

write_start_link(Id, StartId, Graph, File) :-
    first(Graph, Node),
    unpack_node(Node, A, _, _),
    select(Val, A, []),
    write(File, 'Start'),
    write(File, StartId),
    write(File, ' -> '),
    write(File, Val),
    write(File, Id),
    write(File, ';\n').

write_node(_, _, [], _).
write_node(Id, Node, [O|Out], File) :-
    write(File, Node),
    write(File, Id),
    write(File, ' -> '),
    write(File, O),
    write(File, Id),
    write(File, ';\n'),
    write_node(Id, Node, Out, File).

write_graph_sub(_, [], _).
write_graph_sub(Id, [G|Graph], File) :-
    unpack_node(G, A, _, Out),
    select(Node, A, []),
    write_node(Id, Node, Out, File),
    write_graph_sub(Id, Graph, File).

write_graph_sub(Id, [G|Graph], File) :-
    unpack_node(G, A, _, Out),
    select(Node, A, []),
    write_node(Id, Node, Out, File),
    write_graph_sub(Id, Graph, File).

write_graph_header([], _, _).
write_graph_header([G1|Graphs], StartId, File) :-
    graph_id(G1, Id, G2),
    write_start_link(Id, StartId, G2, File),
    write_graph_sub(Id, G2, File),
    write_graph_header(Graphs, StartId, File).

write_graph(Graph, StartId, File) :-
    write(File, 'Start'),
    write(File, StartId),
    write(File, ' [style=filled, fillcolor = yellow]\n'),
    write_graph_header(Graph, StartId, File).

write_dot_sub([], _, _).
write_dot_sub([G|Graph], StartId, File) :-
    write_graph(G, StartId, File),
    incr(StartId, NewId),
    write_dot_sub(Graph, NewId, File).

write_struct(Graphs, File) :-
    write(File, 'digraph G {\n'),
    write_dot_sub(Graphs, 1, File),
    write(File, '}\n'),
    flush_output(File).

write_dot(Graphs, FileName) :-
  setup_call_cleanup(
    open(FileName, write, File),
    write_struct(Graphs, File),
    close(File)).