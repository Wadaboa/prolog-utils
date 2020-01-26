%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%% GENERIC %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Negation as failure
not(X) :-
    X,
    !,
    fail.
not(_).

% Check if an integer is even
even(X) :-
    0 is X mod 2.

% Check if an integer is odd
odd(X) :-
    not(even(X)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% LIST %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Check if the given variable is a list
list([]).
list([_|L]) :-
    list(L).

% Membership of a list
member(H, [H|_]).
member(V, [_|T]) :-
    member(V, T).

% Check if an element is not present in the given list
not_member(V, X) :-
    not(member(V, X)).

% Membership by selection
member_by_if(X, [Y|RS]) :-
    (   X=Y
    ->  true
    ;   member_by_if(X, RS)
    ->  true
    ;   false
    ).

% Number of occurences of an integer in a list
occurrences([], _, O) :-
    O is 0.
occurrences([I|T], I, O) :-
    occurrences(T, I, P),
    O is P+1.
occurrences([_|T], I, O) :-
    occurrences(T, I, O).

% Length of a list
list_length([], 0).
list_length([_|T], L) :-
    list_length(T, M),
    L is M+1.

% First element of a list
first([], []).
first([H|_], H).

% Simple last element of a list
last([H], H) :-
    !.
last([_|T], L) :-
    last(T, L).

% Last element of a list using the reverse list
last_by_reverse(X, L) :-
    reverse(X, R),
    first(R, L).

% Concatenation of lists
append([], Y, Y).
append([XH|XT], Y, [XH|Z]) :-
    append(XT, Y, Z).

% Sum of integers in a list
list_sum([], L) :-
    L is 0.
list_sum([H|T], L) :-
    list_sum(T, M),
    L is M+H.

% Reverse a list
reverse([], []).
reverse([XH|XT], Y) :-
    reverse(XT, Z),
    append(Z, [XH], Y).

% Lists elementwise difference
elementwise_diff([], [], []).
elementwise_diff([XH|XT], [YH|YT], [D|Z]) :-
    D is XH-YH,
    elementwise_diff(XT, YT, Z).

% List euclidean norm
squared_euclidean_norm([], N) :-
    N is 0.
squared_euclidean_norm([H|T], N) :-
    squared_euclidean_norm(T, O),
    N is O+H*H.

euclidean_norm(X, N) :-
    squared_euclidean_norm(X, V),
    N is sqrt(V).

% Find an occurrence of an element in a list and remove it
select(X, [X|T], T) :-
    !.
select(X, [H|T1], [H|T2]) :-
    select(X, T1, T2).

% Find every occurrence of an element in a list and remove it
delete(X, [X|T], Z) :-
    delete(X, T, Z).
delete(X, [H|T], [H|Z]) :-
    H=\=X,
    delete(X, T, Z).
delete(_, [], []) :-
    !.

% Remove the first N elements from a list
remove_prefix(X, N, Y) :-
    list_length(X, L),
    T is L-N,
    T>=0,
    list_length(Y, T),
    append(_, Y, X),
    !.

% Remove the last N elements from a list
remove_suffix(X, N, Y) :-
    list_length(X, L),
    T is L-N,
    T>=0,
    list_length(Y, T),
    append(Y, _, X),
    !.

% Split a list into two lists of equal length
split_two([], [], []).
split_two([A], [A], []).
split_two([A, B|T], [A|P1], [B|P2]) :-
    split_two(T, P1, P2).

% Check if a list of integers is ordered
ordered([_]) :-
    !.
ordered([H|T]) :-
    first(T, F),
    (   H<F
    ;   H is F
    ),
    !,
    ordered(T).

% Check if a list is a permutation of another
permutation([], []) :-
    !.
permutation(X, [H|T]) :-
    append(V, [H|U], X),
    append(V, U, W),
    permutation(W, T).

% Get the first N elements of a list
prefix(_, 0, []).
prefix([H|_], 1, [H]) :-
    !.
prefix([H|T], N, [H|F]) :-
    N>=0,
    X is N-1,
    prefix(T, X, F).

% Get the last N elements of a list
suffix(X, N, Y) :-
    list_length(X, L),
    T is L-N,
    remove_prefix(X, T, Y),
    !.

% Get the last N elements of a list starting from the end
inverse_suffix(X, N, Y) :-
    reverse(X, Z),
    prefix(Z, N, Y).

% Check if a list is made by the same element
same(X, I) :-
    occurrences(X, I, O),
    list_length(X, L),
    L is O.

% Repeat the same element N times in a list
replicate(_, 0, []).
replicate(I, 1, [I]) :-
    !.
replicate(I, N, [I|T]) :-
    N>=0,
    X is N-1,
    replicate(I, X, T).

% Get the maximum in a list of integers
list_max([H], H) :-
    !.
list_max([H|T], M) :-
    list_max(T, N),
    H>=N,
    M is H,
    !.
list_max([H|T], M) :-
    list_max(T, N),
    N>=H,
    M is N,
    !.

% Get the minimum in a list of integers
list_min([H], H) :-
    !.
list_min([H|T], M) :-
    list_min(T, N),
    H=<N,
    M is H,
    !.
list_min([H|T], M) :-
    list_min(T, N),
    N=<H,
    M is N,
    !.

% Count the number of positive integers in a list
positive_integers_num([], N) :-
    N is 0,
    !.
positive_integers_num([H|T], N) :-
    H>=0,
    positive_integers_num(T, M),
    N is M+1,
    !.
positive_integers_num([H|T], N) :-
    H<0,
    positive_integers_num(T, N).

% Extract the positive integers from a list of integers
positive_integers([], []) :-
    !.
positive_integers([H|T], X) :-
    H>=0,
    positive_integers(T, Y),
    append([H], Y, X).
positive_integers([H|T], X) :-
    H<0,
    positive_integers(T, X),
    !.

% Check if two lists are the same
equal([], []).
equal([XH|XT], [YH|YT]) :-
    XH is YH,
    equal(XT, YT).

% Check if the given list is palindrome
palindrome(X) :-
    reverse(X, R),
    equal(X, R).

% Flatten a multi-dimensional list
flatten([X|L], F) :-
    flatten(X, F1),
    flatten(L, F2),
    append(F1, F2, F),
    !.
flatten(X, [X]) :-
    not(list(X)).
flatten([], []) :-
    !.

% Bogo sort
bogo_sort(X, S) :-
    permutation(X, S),
    ordered(S),
    !.

% Bubble sort
bubble_sort(L, L) :-
    ordered(L).
bubble_sort(L1, L2) :-
    append(X, [A, B|Y], L1),
    A>B,
    append(X, [B, A|Y], T),
    bubble_sort(T, L2),
    !.

% Merge two sorted sublists into one sorted list
merge_two([], L, L).
merge_two(L, [], L).
merge_two([A|T1], [B|T2], [A|L2]) :-
    A=<B,
    merge_two(T1, [B|T2], L2),
    !.
merge_two([A|T1], [B|T2], [B|L2]) :-
    A>B,
    merge_two([A|T1], T2, L2),
    !.

% Merge sort
merge_sort([], []).
merge_sort([A], [A]).
merge_sort(L1, L2) :-
    split_two(L1, P1, P2),
    merge_sort(P1, S1),
    merge_sort(P2, S2),
    merge_two(S1, S2, L2),
    !.

% Remove consecutive equal elements of a list
compress([], []).
compress([H], [H]).
compress([H, H|T], Y) :-
    compress([H|T], Y).
compress([H, F|T], [H|Y]) :-
    H=\=F,
    compress([F|T], Y).

% Pack consecutive duplicates of list elements into sublists
pack([], []).
pack(X, Y) :-
    pack(X, [], Y).

pack([H], [], [[H]]).
pack([H], [L], [[H], [L]]) :-
    H=\=L.
pack([H], [H], [[H, H]]).
pack([H, H|T], L, Y) :-
    pack([H|T], [H|L], Y).
pack([H, F|T], L, Y) :-
    H=\=F,
    pack([F|T], [], Z),
    append([[H|L]], Z, Y).

% Run-length encoding of a list
rl_encode(X, Y) :-
    pack(X, P),
    encode(P, Y).
encode([], []).
encode([[H|T1]|T2], Y) :-
    length([H|T1], L),
    encode(T2, Z),
    append([[H, L]], Z, Y).

% Modified run-length encoding of a list
mrl_encode(X, Y) :-
    pack(X, P),
    mod_encode(P, Y).
mod_encode([], []).
mod_encode([[H|T1]|T2], Y) :-
    length([H|T1], L),
    mod_encode(T2, Z),
    L>1,
    append([[H, L]], Z, Y).
mod_encode([[H|T1]|T2], Y) :-
    length([H|T1], L),
    mod_encode(T2, Z),
    L is 1,
    append([H], Z, Y).

% Duplicate the elements of a list
dupli([], []).
dupli([H|T], Y) :-
    dupli(T, Z),
    append([H, H], Z, Y).

% Duplicate the elements of a list a given number of times
dupli(X, N, Y) :-
    dupli(X, N, N, Y).
dupli([], _, _, []).
dupli([_|T], N, 0, Y) :-
    dupli(T, N, N, Y).
dupli([H|XT], N, M, [H|YT]) :-
    M>0,
    L is M-1,
    dupli([H|XT], N, L, YT).

% Extract a slice from a list
slice(X, I, K, S) :-
    slice(X, I, 0, K, S).
slice(_, _, J, K, []) :-
    J is K+1.
slice([_|T], I, J, K, S) :-
    J<I,
    V is J+1,
    slice(T, I, V, K, S).
slice([H|XT], I, J, K, [H|ST]) :-
    J>=I,
    J=<K,
    V is J+1,
    slice(XT, I, V, K, ST).

% Remove the k-th element from a list
remove_at(X, K, Y) :-
    remove_at(X, K, 0, Y).
remove_at([], _, _, []).
remove_at([XH|XT], K, L, [XH|YT]) :-
    L=\=K,
    M is L+1,
    remove_at(XT, K, M, YT).
remove_at([_|XT], K, L, YT) :-
    L=:=K,
    M is L+1,
    remove_at(XT, K, M, YT).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% SET / QUEUE %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Set type
set([], []).
set([H|T], [H|S]) :-
    not_member(H, T),
    !,
    set(T, S).
set([_|T], S) :-
    set(T, S).

% Queue type
queue(L) :-
    list(L).

% Enqueue operation
enqueue(queue(Q1), E, queue(Q2)) :-
    not(queue(E)),
    append(Q1, [E], Q2),
    !.

% Dequeue operation
dequeue(queue(Q1), queue(Q2)) :-
    remove_prefix(Q1, 1, Q2),
    !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% TREE %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Binary tree type
binary_tree(void).
binary_tree(tree(_, L, R)) :-
    binary_tree(L),
    binary_tree(R).

% Test for the empty tree
empty(void).

% In-order visit of a binary tree (Left, Root, Right)
inorder(tree(E, L, R), V) :-
    inorder(L, X),
    inorder(R, Y),
    append(X, [E|Y], V).
inorder(void, []).

% Pre-order visit of a binary tree (Root, Left, Right)
preorder(tree(E, L, R), V) :-
    preorder(L, X),
    preorder(R, Y),
    append([E|X], Y, V).
preorder(void, []).

% Post-order visit of a binary tree (Left, Right, Root)
postorder(tree(E, L, R), V) :-
    postorder(R, Y),
    postorder(L, X),
    append(X, Y, Z),
    append(Z, [E], V).
postorder(void, []).

% Sum of the integer nodes of a binary tree
binary_tree_sum(tree(E, L, R), S) :-
    binary_tree_sum(L, LS),
    binary_tree_sum(R, RS),
    S is E+LS+RS.
binary_tree_sum(void, 0).

% Count the leaves of a binary tree
count_leaves(tree(_, L, R), S) :-
    count_leaves(L, LS),
    count_leaves(R, RS),
    S is LS+RS+1.
count_leaves(void, 0).

% Get the leaves of a binary tree in a list
leaves(tree(L, void, void), [L]) :-
    !.
leaves(tree(_, L, R), S) :-
    leaves(L, LS),
    leaves(R, RS),
    append(LS, RS, S).
leaves(void, []).

% Get the internal nodes of a binary tree
internals(tree(_, void, void), []) :-
    !.
internals(tree(E, L, R), S) :-
    internals(L, LS),
    internals(R, RS),
    not((   empty(L)
        ;   empty(R)
        )),
    append([E|LS], RS, S).
internals(void, []).

% Get every node of a binary tree as a list
nodes(tree(E, L, R), N) :-
    internals(tree(E, L, R), X),
    leaves(tree(E, L, R), Y),
    append(X, Y, S),
    permutation(S, N),
    !.

% Get the height of a binary tree
height(tree(_, void, void), 1) :-
    !.
height(tree(_, L, R), H) :-
    height(L, LH),
    height(R, RH),
    LH>=RH,
    H is LH+1,
    !.
height(tree(_, L, R), H) :-
    height(L, LH),
    height(R, RH),
    LH<RH,
    H is RH+1,
    !.
height(void, 0).

% Collect the nodes at a given level in a binary tree
nodes_at_level(tree(E, _, _), 0, [E]) :-
    !.
nodes_at_level(tree(_, L, R), N, H) :-
    M is N-1,
    nodes_at_level(L, M, LH),
    nodes_at_level(R, M, RH),
    append(LH, RH, H),
    !.
nodes_at_level(void, _, []).
