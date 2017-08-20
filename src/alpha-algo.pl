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

