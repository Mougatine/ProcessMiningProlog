clearall :-
  retractall(has_parallel(_,_)),
  retractall(has_choice(_,_)),
  retractall(has_causality(_,_)),
  retractall(has_reverse_causality(_,_)).

:- dynamic (has_parallel/2,
            has_choice/2,
            has_causality/2,
            has_reverse_causality/2),
   clearall.

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
% Alpha algorithm
%==

%-----------------------------------------------------------------------------
% Create the list of all the transition Tl.
%--

% Get all the transition from a list.
create_Tl_sub([A, B], [[A,B]]).
create_Tl_sub([A, B | R], [[A,B] | Tl]) :-
  create_Tl_sub([B | R], Tl).

% Get all the transition from a list of list.
create_Tl([], []).
create_Tl([Sub | R], Tl) :-
  create_Tl(R, Ret_R),
  create_Tl_sub(Sub, Ret_sub),
  append_list(Ret_sub, Ret_R, Tl).

%-----------------------------------------------------------------------------
% Create the list of all initial state Ti.
%--

create_Ti([], []).
create_Ti([[I|_] | R], Ti) :-
  create_Ti(R, Ret_R),
  unique_add(I, Ret_R, Ti).

%-----------------------------------------------------------------------------
% Create the list of all final state To.
%--

create_To_sub([O], O).
create_To_sub([_|R], O) :-
  create_To_sub(R, O).

create_To([], []).
create_To([L|R], To) :-
  create_To_sub(L, O),
  create_To(R, Ret_R),
  unique_add(O, Ret_R, To).

%-----------------------------------------------------------------------------
% Check if a direct succession exist (equiv to transition).
%
% Direct succession: A>B
% iff theta=t1 ... tn, it exist ti=A et ti+1=B

%--

% does A>B ?
direct_succession(A, B, Tl) :-
  member([A,B], Tl).

%-----------------------------------------------------------------------------
% Check if there is a causality of A to B.
%
% Causality: A->B
% A->B iff A>B and not B>A.
%--

% Does A->B ?
causality(A, B, Tl) :-
  direct_succession(A, B, Tl),
  not(direct_succession(B, A, Tl)).

reverse_causality(A, B, Tl) :-
  direct_succession(B, A, Tl),
  not(direct_succession(A, B, Tl)).

%-----------------------------------------------------------------------------
% Check if A and B are concurent.
%
% Parallel: A||B
% A||B iff A>B and B>A
%--

% Does A||B ?
parallel(A, B, Tl) :-
  direct_succession(A, B, Tl),
  direct_succession(B, A, Tl).

%-----------------------------------------------------------------------------
% Check if there is a choice between A and B.
%
% Choice: A#B
% A#B iff not A>B and not B>A.
%--

% Does A#B ?
choice(A, B, Tl) :-
  not(direct_succession(A, B, Tl)),
  not(direct_succession(B, A, Tl)).

%-----------------------------------------------------------------------------
% Check if there is a loop of size 2 with A and B.
%
% WARNING: This came from the extended algorithm
%
% short loop: A@B
% A@B iff A^B and B^A
% with
% A^B iff theta=t1 ... tn, it exist ti=ti+2=A et ti+1=B
%--

% Does A^B in that list ?
half_loop_sub(_, _, [_,_]).
half_loop_sub(A, B, [A, B, A | _]).
half_loop_sub(A, B, [_|L]) :-
  half_loop_sub(A, B, L).

% Does A^B in that list of list ?
half_loop(A, B, [L|_]) :-
  half_loop_sub(A, B, L).
half_loop(A, B, [_|R]) :-
  half_loop(A, B, R).

% Does A@B in the logs ?
loop_of_2(A, B, EventLog) :-
  half_loop(A, B, EventLog),
  half_loop(B, A, EventLog).

%-----------------------------------------------------------------------------
% Check if there is a loop of size 1 with A.
%
% HARD WARNING: This came from myself.
%--

loop_of_1(A, Tl) :-
  direct_succession(A, A, Tl).

%-----------------------------------------------------------------------------
% Create database (array of relation)
%
% Add needed relation into the dynamic database of prolog.
%--

add_relation(A, B, Tl) :-
  parallel(A, B, Tl),
  assert(has_parallel(A, B)).
add_relation(A, B, Tl) :-
  choice(A, B, Tl),
  assert(has_choice(A, B)).
add_relation(A, B, Tl) :-
  causality(A, B, Tl),
  assert(has_causality(A, B)).
add_relation(A, B, Tl) :-
  reverse_causality(A, B, Tl),
  assert(has_reverse_causality(A, B)).

create_database_rec([], _, _).
create_database_rec(_, [], _).
create_database_rec([X1|L1], [X2|L2], Tl) :-
  add_relation(X1, X2, Tl),
  create_database_rec([X1|L1], L2, Tl),
  create_database_rec(L1, [X2|L2], Tl).
create_database(Alphabet, Tl) :-
  create_database_rec(Alphabet, Alphabet, Tl).

%-----------------------------------------------------------------------------
% Set of operations to check if a relation in true for a given list.
%--

% Check that `has_choice(E, ...)` is true for all the member of the list.
%
% has_only_choice_in(Element, List).
has_only_choice_in(_, []).
has_only_choice_in(E, [X|L]) :-
  E \== X,
  has_choice(E, X),
  has_only_choice_in(E, L).

% Check that `has_causality(E, ...)` is true for all the member of the list.
%
% has_only_causality_in(Element, List).
has_only_causality_in(_, []).
has_only_causality_in(E, [X|L]) :-
  E \== X,
  has_causality(E, X),
  has_only_causality_in(E, L).

% Check that `has_reverse_causality(E, ...)` is true for all the member of the list.
%
% has_only_reverse_causality_in(Element, List).
has_only_reverse_causality_in(_, []).
has_only_reverse_causality_in(E, [X|L]) :-
  E \== X,
  has_reverse_causality(E, X),
  has_only_causality_in(E, L).

%-----------------------------------------------------------------------------
% Create Xl, a set pairs representing state for the petri's net.
%
% A state is represent by o pair of two sets.
% The first set represent all the activities that can be input of this state.
% The second set represent all the activities that can be output of this state.
%
% Xl, is a list of pair, representing set. However, it may contains several
% times the same state because a pair can be include in another.
%
% Yl, is Xl from which all the states are only represented once. (All the pair
% that are include in another are removed).
%--

% Get a list of all element B where A->B is true and an another where A->B is
% false.
%
% get_causality(Element, Alphabet, CausalityList, NonCausalityList).
get_causality(_, [], [], []).
get_causality(X, [A|R], [A|L1], L2) :-
  X \== A,
  has_causality(X, A),
  get_causality(X, R, L1, L2).
get_causality(X, [A|R], L1, [A|L2]) :-
  X \== A,
  get_causality(X, R, L1, L2).
get_causality(X, [_|R], L1, L2) :-
  get_causality(X, R, L1, L2).

% Create pair with an element E and a list of elements C, return a list of pairs
% [[E],[C]].
%
% create_pair(Element, List of C, List of pair [[[E],[C]] | ...])
create_pair(_, [], []).
create_pair(E, [C|R], [ [[E],[C]] | L ]) :-
  create_pair(E, R, L).

% Try to add an element C in the second set of the pairs. The element is add iff
% for all element of the sets S->C and C#E is true.
extend_end_sub(_, [], []).
extend_end_sub(C, [[S,E] | R], [[S,E] | Ret]) :-
  (not(has_only_choice_in(C, E));
   not(has_only_reverse_causality_in(C, S))),
  extend_end_sub(C, R, Ret).
extend_end_sub(C, [[S,E] | R], [[S,E],[S,[C|E]] | Ret]) :-
  extend_end_sub(C, R, Ret).
% Try to create pair that contains more informations. To do so, it try to add
% each element to the second set of each pair.
%
% extend_end(List of element, list of pairs, new list of pairs).
extend_end([], R, R).
extend_end([C|L], G, Ret) :-
  extend_end_sub(C, G, R1),
  extend_end(L, R1, Ret).

% Try to add an element C in the first set of the pairs. The element is add iff
% for all element of the sets C#S and S->E is true.
extend_start_sub(_, [], []).
extend_start_sub(C, [[S,E] | R], [[S,E] | Ret]) :-
  (not(has_only_choice_in(C, S));
   not(has_only_causality_in(C, E))),
  extend_start_sub(C, R, Ret).
extend_start_sub(C, [[S,E] | R], [[S,E],[[C|S],E] | Ret]) :-
  extend_start_sub(C, R, Ret).
% Try to create pair that contains more informations. To do so, it try to add
% each element to the first set of each pair.
%
% extend_start(List of element, list of pairs, new list of pairs).
extend_start([], R, R).
extend_start([C|L], G, Ret) :-
  extend_start_sub(C, G, R1),
  extend_start(L, R1, Ret).

% Create Xl for on element E.
%
% create_Xl_for(element E, the alphabet, Xl).
create_Xl_for(E, Alphabet, Xl) :-
  get_causality(E, Alphabet, Causality, NoCausality),
  create_pair(E, Causality, S1),
  extend_end(Causality, S1, S2),
  extend_start(NoCausality, S2, Xl).

%-----------------------------------------------------------------------------
% Create Yl, a set pairs representing state for the petri's net.
%
% Yl, is Xl from which all the states are only represented once. (All the pair
% that are include in another are removed).
%--

% Check if a list A is contained by a list B.
%
% list_is_included(A, B).
list_is_included([], _).
list_is_included([X|R], L) :-
  member(X, L),
  list_is_included(R, L).

% Check if a pair A is contained by a pair B.
%
% pair_is_included(A, B).
pair_is_included([S1,E1], [[S2,E2] | _]) :-
  list_is_included(S1, S2),
  list_is_included(E1, E2).
pair_is_included([S1,E1], [[_,_] | R]) :-
  pair_is_included([S1,E1], R).

% Remove all the pairs that are included an other pair.
%
% remove_included_pair(list of pairs, new list of pairs)
remove_included_pair([], []).
remove_included_pair([[S,E] | R], Ret) :-
  pair_is_included([S,E], R),
  remove_included_pair(R, Ret).
remove_included_pair([[S,E] | R], [[S,E] | Ret]) :-
  remove_included_pair(R, Ret).

% For each element of the alphabet, create Xl for that element. Then append all
% the Xl in one list and remove all the pairs included in an another.
%
% create_Yl_sub(the alphabet, the alphabet, Yl).
create_Yl_sub([], _, []).
create_Yl_sub([A|L], Alphabet, Yl) :-
  create_Xl_for(A, Alphabet, Yl1),
  create_Yl_sub(L, Alphabet, Yl2),
  append_list(Yl1, Yl2, Yl3),
  remove_included_pair(Yl3, Yl).
% Create Yl.
%
% Create_Yl(the alphabet, Yl).
create_Yl(Alphabet, Yl) :-
  create_Yl_sub(Alphabet, Alphabet, Yl).

%-----------------------------------------------------------------------------
% Alpha algorithm.
%
% Take logs as input and return Ti (the set of initial activities), Yl (a list
% of pairs of set, representing states of the petri's net) and To (the set of
% final activities).
%--

alpha_algo(Logs, Ti, Yl, To) :-
  clearall,
  create_alphabet(Logs, Alphabet),
  create_Ti(Logs, Ti),
  create_To(Logs, To),
  create_Tl(Logs, Tl),
  create_database(Alphabet, Tl),
  create_Yl(Alphabet, Yl).

%-----------------------------------------------------------------------------
% TESTS
%--

run_test(Logs) :-
  alpha_algo(Logs, Ti, Yl, To),
  write('Ti= '), write(Ti), nl,
  write('Yl= '), write(Yl), nl,
  write('To= '), write(To), nl.

% Ti should contains [a]
% Yl should contains [[[a],[b,e]], [[a],[c,e]], [[b,c],[d]], [[c,e],[d]]]
% To should contains [d]
alpha_test1 :-
  Logs=[[a,b,c,d],[a,c,b,d],[a,e,d]],
  run_test(Logs).

% Ti should contains [a]
% Yl should contains [[[b],[c], [[b],[d]], [[c],[e]], [[d],[e]], [[e],[f,g]],
%                     [[a,f],[b]]
% To should contains [g]
alpha_test2 :-
  Logs=[[a,b,c,d,e,g], [a,b,d,c,e,f,b,c,d,e,g],
       [a,b,c,d,e,f,b,c,d,e,f,b,d,c,e,g]],
  run_test(Logs).


alpha_test3 :-
  Logs=[[a,b,g,e,d,f],[a,g,b,e,d,f],[a,b,g,d,e,f],[a,g,b,d,e,f],
        [a,c,g,e,d,f],[a,g,c,e,d,f],[a,c,g,d,e,f],[a,g,c,d,e,f]],
  run_test(Logs).

alpha_test4 :-
  Logs=[[a,b,d,z],[a,d,b,z],[a,e,f,z],[a,f,e,z]],
  run_test(Logs).

alpha_test5 :-
  Logs=[[a,b,e,d,f],[a,b,d,e,f],[a,d,b,e,f],
        [a,c,e,d,f],[a,c,d,e,f],[a,d,c,e,f]],
  run_test(Logs).
