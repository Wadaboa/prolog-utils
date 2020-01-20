%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% Logic Programming %%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Query with ?- queens(100, Q).
queens(N, Queens) :-
	queens_list(N, Q),
	queens(Q, [], Queens).
queens_list(0, []).
queens_list(N, [N|Ns]):-
	N > 0,
	N1 is N - 1,
	queens_list(N1, Ns).
queens([], Qs, Qs).
queens(U, P, Qs) :-
	select(Q, U, U1),
	no_attack(P, Q, 1),
	queens(U1, [Q|P], Qs).
no_attack([], _Q, _Nb).
no_attack([Y|Ys], Queen, Nb) :-
	Queen =\= Y + Nb,
	Queen =\= Y - Nb,
	Nb1 is Nb + 1,
	no_attack(Ys, Queen, Nb1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% Constraint Logic Programming %%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(clpfd)).

% Query with ?- queens_clp(100, Q), labeling([ff], Q).
queens_clp(N, Q) :-
	length(Q, N),
	Q ins 1..N,
	safe_queens(Q).
safe_queens([]).
safe_queens([Q1|Q]) :-
	safe_queens(Q, Q1, 1),
	safe_queens(Q).
safe_queens([], _, _).
safe_queens([Q1|Q], Q0, D0) :-
	Q0 #\= Q1,
	abs(Q0 - Q1) #\= D0,
	D1 #= D0 + 1,
	safe_queens(Q, Q0, D1).
