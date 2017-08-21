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

get_log1(L) :-
  L=[[a,b,c,d],[a,c,b,d],[a,e,d]].

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
%--

% Direct succession: A>B
% iff theta=t1 ... tn, it exist ti=A et ti+1=B

% does A>B ?
direct_succession(A, B, Tl) :-
  member([A,B], Tl).

%-----------------------------------------------------------------------------
% Check if there is a causality of A to B.
%--

% Causality: A->B
% A->B iff A>B and not B>A.

% Does A->B ?
causality(A, B, Tl) :-
  direct_succession(A, B, Tl),
  not(direct_succession(B, A, Tl)).

%-----------------------------------------------------------------------------
% Check if A and B are concurent.
%--

% Parallel: A||B
% A||B iff A>B and B>A

% Does A||B ?
parallel(A, B, Tl) :-
  direct_succession(A, B, Tl),
  direct_succession(B, A, Tl).

%-----------------------------------------------------------------------------
% Check if there is a choice between A and B.
%--

% Choice: A#B
% A#B iff not A>B and not B>A.

% Does A#B ?
choice(A, B, Tl) :-
  not(direct_succession(A, B, Tl)),
  not(direct_succession(B, A, Tl)).

%-----------------------------------------------------------------------------
% Check if there is a loop of size 2 with A and B.
%
% WARNING: This came from the extended algorithm
%--

% short loop: A@B
% A@B iff A^B and B^A
% with
% A^B iff theta=t1 ... tn, it exist ti=ti+2=A et ti+1=B

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

