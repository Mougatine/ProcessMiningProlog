:- consult('src/alpha-algo.pl').

:- dynamic(block/2).

size_of([_], 1).
size_of([_|L], R+1) :-
  !,
  size_of(L, R).
size_of(_, 0).

reverse_sub([], Acc, Acc).
reverse_sub([X|L], Acc, R) :-
  reverse_sub(L, [X|Acc], R).
reverse(L, R) :-
  reverse_sub(L, [], R).

flatten([], []).
flatten([X], X).

check_equiv(A, B, C, A, B, C).

%-----------------------------------------------------------------------------
% Fuse
%--

fuse_rec(_, [], []).
fuse_rec(E, [[S,E] | L], [S|R]) :-
  fuse_rec(E, L, R).
fuse_rec(E, [[_,_] | L], R) :-
  fuse_rec(E, L, R).

fuse_sub([], _, _, []).
fuse_sub([[_,E] | L], List, Visited, [[R_f,E] | R]) :-
  not(member(E, Visited)),
  fuse_rec(E, List, R_f),
  fuse_sub(L, List, [E|Visited], R).
fuse_sub([[_,E] | L], List, Visited, R) :-
  member(E, Visited),
  fuse_sub(L, List, Visited, R).

fuse(L, R) :-
  fuse_sub(L, L, [], R).

%-----------------------------------------------------------------------------
% Smooth
%--

smooth([], []).
smooth([X|L], [R_x|R_l]) :-
  size_of(X, Len),
  Len > 0,
  smooth(X, R_x),
  smooth(L, R_l).
smooth([[X]|L], [X|R]) :-
  size_of(X, 0),
  smooth(L, R).
smooth([X|L], [X|R]) :-
  smooth(L, R).

%-----------------------------------------------------------------------------
% Find operators.
%--

find_ALT([], [], []).
find_ALT([[S,E] | L], [[S,E] | R_or], R_Yl) :-
  size_of(E, Len),
  Len > 1,
  find_ALT(L, R_or, R_Yl).
find_ALT([X|L], R_or, [X|R_Yl]) :-
  find_ALT(L, R_or, R_Yl).

get_ALT(Yl, Ret, R_Yl) :-
  find_ALT(Yl, R_alt, R_Yl),
  smooth(R_alt, Ret).

%--

find_PAR_sub([_,E], [], [E], []).
find_PAR_sub([S1,E1], [[S2,E2] | R], [E2 | R_sub], R_Yl) :-
  S1 == S2,
  E1 \== E2,
  find_PAR_sub([S1,E1], R, R_sub, R_Yl).
find_PAR_sub([S1,E1], [X|R], R_sub, [X|R_Yl]) :-
  find_PAR_sub([S1,E1], R, R_sub, R_Yl).

find_PAR([], [], []).
find_PAR([[S,E] | L], [[S,R_sub] | R_and], R_Yl) :-
  find_PAR_sub([S,E], L, R_sub, R_L),
  size_of(R_sub, Len),
  Len > 1,
  find_PAR(R_L, R_and, R_Yl).
find_PAR([X|L], R_and, [X|R_Yl]) :-
  find_PAR(L, R_and, R_Yl).

get_PAR(Yl, Ret,  R_Yl) :-
  find_PAR(Yl, R_par, R_Yl),
  fuse(R_par, Ret_f),
  smooth(Ret_f, Ret).

%--

find_SEQ([], [], []).
find_SEQ([[S,E] | L], [[S,E] | R_seq], R_Yl) :-
  size_of(S, Len_S),
  Len_S == 1,
  size_of(E, Len_E),
  Len_E == 1,
  find_SEQ(L, R_seq, R_Yl).
find_SEQ([[S,E] | L], R_seq, [[S,E] | R_Yl]) :-
  find_SEQ(L, R_seq, R_Yl).

get_SEQ(Yl, Ret, R_Yl) :-
  find_SEQ(Yl, R_seq, R_Yl),
  smooth(R_seq, Ret).

%-----------------------------------------------------------------------------
% Find join of operator.
%--

find_join_ALT([], [], []).
find_join_ALT([[S,E] | L], [[S,E] | R_alt], R_Yl) :-
  size_of(S, Len),
  Len > 1,
  find_join_ALT(L, R_alt, R_Yl).
find_join_ALT([X|L], R_alt, [X|R_Yl]) :-
  find_join_ALT(L, R_alt, R_Yl).

%--

find_join_PAR_sub([S,_], [], [S], []).
find_join_PAR_sub([S1,E1], [[S2,E2] | R], [S2 | R_sub], R_Yl) :-
  S1 \== S2,
  E1 == E2,
  find_join_PAR_sub([S1,E1], R, R_sub, R_Yl).
find_join_PAR_sub([S1,E1], [X|R], R_sub, [X|R_Yl]) :-
  find_join_PAR_sub([S1,E1], R, R_sub, R_Yl).

find_join_PAR([], [], []).
find_join_PAR([[S,E] | L], [[E,R_sub] | R_and], R_Yl) :-
  find_join_PAR_sub([S,E], L, R_sub, R_L),
  size_of(R_sub, Len),
  Len > 1,
  find_join_PAR(R_L, R_and, R_Yl).
find_join_PAR([X|L], R_and, [X|R_Yl]) :-
  find_join_PAR(L, R_and, R_Yl).

%-----------------------------------------------------------------------------
% Translate to Seq, Par, Alt.
%--

starting_with(_, [], []).
starting_with(S1, [[S2,E] | L], [E | Ret]) :-
  S1 == S2,
  starting_with(S1, L, Ret).
starting_with(S1, [[S2,E] | L], [E | Ret]) :-
  [S1] == S2,
  starting_with(S1, L, Ret).
starting_with(S1, [[S2,E] | L], [E | Ret]) :-
  S1 == [S2],
  starting_with(S1, L, Ret).
starting_with(S, [[_,_] | L], Ret) :-
  starting_with(S, L, Ret).

get_starting_with(E, Par, Alt, Seq, R_par2, R_alt, R_seq) :-
  starting_with(E, Alt, R_alt),
  starting_with(E, Par, R_par),
  flatten(R_par, R_par2),
  starting_with(E, Seq, R_seq).

has_starting_with(E, Par, Alt, Seq, A1, B1, C1) :-
  get_starting_with(E, Par, Alt, Seq, A2, B2, C2),
  !,
  check_equiv(A1, B1, C1, A2, B2, C2).


%--

% case END
follow_alt_sub([], Par, Alt, Seq, Visited) :-
  write(')'),
  % Reverse ? Or write a spec equal ?
  reverse(Visited, Rev),
  new_block(Rev, Par, Alt, Seq).
% case simple X
follow_alt_sub([X|L], Par, Alt, Seq, Visited) :-
  size_of(X, Len), Len =< 1,
  has_starting_with(X, Par, Alt, Seq, [], [], []),
  write(X), write(','),
  follow_alt_sub(L, Par, Alt, Seq, [X|Visited]).
% case X start a SEQ
follow_alt_sub([X|L], Par, Alt, Seq, Visited) :-
  size_of(X, Len), Len =< 1,
  has_starting_with(X, Par, Alt, Seq, [], [], _),
  write('Seq('),
  follow_seq(X, Par, Alt, Seq),
  write(','),
  follow_alt_sub(L, Par, Alt, Seq, [X|Visited]).
% case X start an PAR
follow_alt_sub([X|L], Par, Alt, Seq, Visited) :-
  size_of(X, Len), Len =< 1,
  has_starting_with(X, Par, Alt, Seq, _, [], []),
  write('Par('),
  follow_par(X, Par, Alt, Seq),
  write(','),
  follow_alt_sub(L, Par, Alt, Seq, [X|Visited]).
% case X is a PAR
follow_alt_sub([X|L], Par, Alt, Seq, Visited) :-
  write('Par('),
  follow_par(X, Par, Alt, Seq),
  write(','),
  follow_alt_sub(L, Par, Alt, Seq, [X|Visited]).

follow_alt(L, Par, Alt, Seq) :-
  follow_alt_sub(L, Par, Alt, Seq, []).

%--

% case END
follow_par_sub([], Par, Alt, Seq, Visited) :-
  write(')'),
  % Reverse ? Or write a spec equal ?
  reverse(Visited, Rev),
  new_block(Rev, Par, Alt, Seq).
% case simple X
follow_par_sub([X|L], Par, Alt, Seq, Visited) :-
  size_of(X, Len), Len =< 1,
  has_starting_with(X, Par, Alt, Seq, [], [], []),
  write(X), write(','),
  follow_par_sub(L, Par, Alt, Seq, [X|Visited]).
% case X start a SEQ
follow_par_sub([X|L], Par, Alt, Seq, Visited) :-
  size_of(X, Len), Len =< 1,
  has_starting_with(X, Par, Alt, Seq, [], [], _),
  write('Seq('),
  follow_seq(X, Par, Alt, Seq),
  write(','),
  follow_par_sub(L, Par, Alt, Seq, [X|Visited]).
% case X start an ALT
follow_par_sub([X|L], Par, Alt, Seq, Visited) :-
  size_of(X, Len), Len =< 1,
  has_starting_with(X, Par, Alt, Seq, _, [], []),
  write('Alt('),
  follow_alt(X, Par, Alt, Seq),
  write(','),
  follow_par_sub(L, Par, Alt, Seq, [X|Visited]).
% case X is a ALT
follow_par_sub([X|L], Par, Alt, Seq, Visited) :-
  write('Alt('),
  follow_alt(X, Par, Alt, Seq),
  write(','),
  follow_par_sub(L, Par, Alt, Seq, [X|Visited]).

follow_par(L_par, Par, Alt, Seq) :-
  follow_par_sub(L_par, Par, Alt, Seq, []).

%--

% Case END
follow_seq(X, Par, Alt, Seq) :-
  has_starting_with(X, Par, Alt, Seq, [], [], []),
  write(X), write(')').
% Case simple X
follow_seq(X, Par, Alt, Seq) :-
  has_starting_with(X, Par, Alt, Seq, [], [], [E]),
  write(X), write(','),
  % Remove [E] from Seq.
  follow_seq(E, Par, Alt, Seq).
% Case X start a PAR or Alt
follow_seq(X, Par, Alt, Seq) :-
  has_starting_with(X, Par, Alt, Seq, _, _, _),
  write(X),
  new_block(X, Par, Alt, Seq),
  write(')').

%--

new_block(I, Par, Alt, Seq) :-
  has_starting_with(I, Par, Alt, Seq, [], _, [E]),
  % Remove [E] from Seq.
  % Do not write Seq, handle in higher call.
  follow_seq(E, Par, Alt, Seq).
new_block(I, Par, Alt, Seq) :-
  has_starting_with(I, Par, Alt, Seq, [], L_alt, []),
  L_alt \== [],
  % Remove L_par from par.
  write('Alt('),
  follow_alt(L_alt, Par, Alt, Seq).
new_block(I, Par, Alt, Seq) :-
  has_starting_with(I, Par, Alt, Seq, L_par, _, []),
  L_par \== [],
  % Remove L_par from par.
  write('Par('),
  follow_par(L_par, Par, Alt, Seq).
new_block(I, Par, Alt, Seq) :-
  has_starting_with(I, Par, Alt, Seq, [], [], []).

%--

to_model_sub([X], Par, Alt, Seq) :-
  write('Seq('),
  follow_seq([X], Par, Alt, Seq).
% Make to model for Alt and Par.

to_model(Logs) :-
  alpha_algo(Logs, Ti, Yl, _),
  get_PAR(Yl, Par, _),
  get_ALT(Yl, Alt, _),
  get_SEQ(Yl, Seq, _),
  to_model_sub(Ti, Par, Alt, Seq).

%-----------------------------------------------------------------------------
% What is this ?
%--
split_log(_, []).
split_log(I, [X|L]) :-
  block(X, P),
  I == P,
  II is I + 1,
  split_log(II, L).
split_log(I, [X|L]) :-
  block(X, P),
  I < P,
  retract(block(X, P)),
  asserta(block(X, I)),
  II is I + 1,
  split_log(II, L).
split_log(I, [X|L]) :-
  block(X, P),
  I > P,
  split_log(I, L).
split_log(I, [X|L]) :-
  asserta(block(X, I)),
  II is I + 1,
  split_log(II, L).

split_logs([]).
split_logs([X|L]) :-
  split_log(0, X),
  split_logs(L).

create_cut(_, [], []).
create_cut(I, [X|L], [X | Ret]) :-
  block(X, I),
  create_cut(I, L, Ret).
create_cut(I, [_|L], Ret) :-
  create_cut(I, L, Ret).
