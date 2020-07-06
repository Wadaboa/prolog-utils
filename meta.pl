% Logical implication
% implies(a(1), b(1)). should return false.
implies(a(X), b(Y)) :- call(a(X)), call(\+b(Y)), !, fail.
implies(a(X), b(Y)).

a(1).
a(2).
b(2).

% If, then, else
if_then_else(Cond, Then, Else) :- call(Cond), !, call(Then).
if_then_else(Cond, Then, Else) :- call(Else).

% Iterate over the elements Pred that satisfy Cond
iterate(Cond, Pred) :- call(Cond), verify(Pred), fail.
iterate(Cond, Pred).
verify(Pred) :- call(Pred), !.

% setof(X, p(X), S). (No repetitions)
% bagof(X, p(X), S). (Possible repetitions)
% findall(X, p(X1, ..., XN), S). (= bagof(X1, (X2, ..., XN)^p(X1, ..., XN), S).)
% setof(X, p(X, Y), S). returns "The set of X's s.t. p(X, Y) holds", where Y is binded to every possible value.
% setof(X, Y^p(X, Y), S). returns "The set of X's s.t. there exists a Y s.t. p(X, Y) holds", without binding Y.
father(giovanni, mario). 
father(giovanni, giuseppe). 
father(mario, paola). 
father(mario, aldo). 
father(giuseppe, maria).

% Exceptions and default rules
fly(X) :- penguin(X), !, fail.
fly(X) :- ostrich(X), !, fail.
fly(X) :- bird(X).

bird(ciccio).
bird(pippo).
bird(pluto).
penguin(ciccio).
ostrich(pippo).

% Properties of terms
% var(Term). (True if Term is currently a variable)
% nonvar(Term). (True if Term is currently not a free variable)
% number(Term). (True if Term is currently a number)
% ground(Term). (True if Term currently holds no free variables)

% Term =.. List
% ?- foo(hello, X) =.. List.
% List = [foo, hello, X]
% ?- Term =.. [baz, foo(1)]. 
% Term = baz(foo(1))

% clause(Head, Body). 
% Unification with a clause whose head is Head and whose body is Body
% Head must be instantiated to a non-numeric term

% Vanilla meta-interpreter
% solve((b, c, d)) calls solve(a) and solve((c, d))
solve(true) :- !.
solve((A,B)) :- !, solve(A), solve(B). 
solve(A) :- clause(A, B), solve(B).

a :- b, c, d.
b :- d.
c.
d.

% Meta-interpreter with number of steps required
solve_step(true, 0) :- !.
solve_step((A, B), S) :- !, solve_step(A, S1), solve_step(B, S2), S is S1 + S2.
solve_step(A, S) :- clause(A, B), solve_step(B, S1), S is S1 + 1.

% Meta-interpreter with certainty scores
solve_cf(true, 100) :- !.
solve_cf((A, B), CF) :- !, solve_cf(A, CF1), solve_cf(B, CF2), min_list([CF1, CF2], CF).
solve_cf(A, CF) :- rule(A, B, CF1), solve_cf(B, CF2), CF is (CF1 * CF2) / 100.

rule(a, (b,c), 10).
rule(a, d, 90).
rule(b, true, 100).
rule(c, true, 50).
rule(d, true, 100).

% Simple expert system
solve_exp(true, []) :- !.
solve_exp((A,B), Z) :- !, solve_exp(A, S1), solve_exp(B, S2), append(S1, S2, Z).
solve_exp(A, S) :- clause(A, B), solve_exp(B, S).
solve_exp(A, [A]) :- \+clause(A, B).

good_pet(X) :- bird(X), small(X).
good_pet(X) :- cuddly(X), yellow(X).
bird(X) :- has_feathers(X), tweets(X).
yellow(tweety).

% Meta-interpreter that prints sub-goals
solve_print(true):- !.
solve_print((A,B)):- !, solve_print(A), solve_print(B). 
solve_print(A) :-
	write('Solving: '), write(A), nl,
	clause(A, B),
	write('Selected rule: '), write(A), write(' :- '), write(B), nl, 
	solve_print(B),
	write('Solved: '), write(B), nl.

p(X) :- q(X), z(X), w(X). 
q(X) :- z(X), w(1).
q(1).
q(2).
z(1).
w(1).

% Meta-interpreter that prints sub-goals and tabs them depending on their depth
solve_print_tab(A) :- solve_print_tab(A, 1).
solve_print_tab(true, _):- !.
solve_print_tab((A,B), D):- !, solve_print_tab(A, D), solve_print_tab(B, D). 
solve_print_tab(A, D) :-
	tab(D), write('Solving: '), write(A), nl,
	clause(A, B),
	D1 is D + 2,
	tab(D1), write('Selected rule: '), write(A), write(' :- '), write(B), nl, 
	solve_print_tab(B, D1),
	tab(D1), write('Solved: '), write(B), nl.

% Meta-interpreter that returns the list of sub-goals
solve_sub(true, []) :- !. 
solve_sub((A,B), S) :- !, solve_sub(A, S1), solve_sub(B, S2), append(S1, S2, S).
solve_sub(A, [A|T]) :- clause(A,B), solve_sub(B, T).

% Meta-interpreter with depth-limited search
solve_dls(G, M) :- solve_dls(G, 0, M).
solve_dls(true, _, _) :- !.
solve_dls(_, D, L) :- D > L, !, fail.
solve_dls((A,B), D, L) :- !, solve_dls(A, D, L), solve_dls(B, D, L).
solve_dls(A, _, _) :- predicate_property(A, built_in), !, call(A).
solve_dls(A, D, L) :- clause(A,B), D1 is D + 1, solve_dls(B, D1, L).

natnum(0).
natnum(s(X)) :- natnum(X).

% Meta-interpreter with iterative-deepening
solve_id(G) :- length(_, N), solve_dls(G, N).

% Dinamically modify the KB (It alters its declarative semantic)
% assert(T). (Clause T is added in an unspecified position in the KB)
% asserta(T). (Clause T is added at the top of the KB)
% assertz(T). (Clause T is added at the bottom of the KB)
% retract(T). (The first clause that unifies with T is removed from the KB) -> e.g. retract((b(X) :- BODY)), retract(a(X)).
% abolish(T, N) (Remove every clause in the KB with arity N that unifies with T) -> e.g. abolish(a, 1).

% Context-free grammar parser
% G -> I + G | I
% I -> l
% e.g. g([l,+,l], []).
g(ListIn, ListOut):- 
	i(ListIn, ListTemp),
	plus(ListTemp, ListTemp1), 
	g(ListTemp1, ListOut). 
g(ListIn, ListOut):- i(ListIn, ListOut).
i(ListIn, ListOut):- l(ListIn, ListOut).
l([l| List],List).
plus([+|List],List).

% Definite-clause grammars
% E -> T + E | T
% T -> a
% phrase(StartingSymbol, List). (e.g. phrase(e, [a,+,a]).)
e --> t, plus, e. 
e --> t.
t --> [a]. 
plus --> [+].

% Palindrome grammar (DCG)
start_dcg --> [a], start_dcg, [a].
start_dcg --> [b], start_dcg, [b].
start_dcg --> [].

% Palindrome grammar (Standard Prolog)
start_std(I, O) :- letter_a(I, T1), start_std(T1, T2), letter_a(T2, O).
start_std(I, O) :- letter_b(I, T1), start_std(T1, T2), letter_b(T2, O).
start_std(H, H). % This is the empty production
letter_a([a|T], T).
letter_b([b|T], T).
