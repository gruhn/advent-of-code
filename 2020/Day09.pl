:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).

numbers([])     --> [].
numbers([N|Ns]) --> integer(N), eol, numbers(Ns).

puzzle_input(Ns) :-
    phrase_from_file(numbers(Ns), "input/09.txt").

not_member(_, []).
not_member(X, [Y|Ys]) :-
    dif(X, Y), not_member(X, Ys).

add(A, B, C) :- C #= A + B.

list_pair_sums([])     --> [].
list_pair_sums([X|Xs]) --> { maplist(add(X), Xs, Ys) }, 
    Ys, list_pair_sums(Xs).

numbers_invalid([X|Xs], Y) :-
    length(Pre, 25),
    append(Pre, [Y|_], [X|Xs]),
    phrase(list_pair_sums(Pre), Sums),
    not_member(Y, Sums).
numbers_invalid([X|Xs], Z) :-
    length(Pre, 25),
    append(Pre, [Y|_], [X|Xs]),
    phrase(list_pair_sums(Pre), Sums),
    member(Y, Sums),
    numbers_invalid(Xs, Z).

list_infix(List, Infix) :-
    append(_, Suffix, List),
    append(Infix, _, Suffix).

numbers_sum_infix(Xs, Y, Infix) :-
    length(Infix, N),
    N #> 1,
    sum(Infix, #=, Y),
    list_infix(Xs, Infix).

min_and_max([V], A, B) :-
    V #= A, V #= B.
min_and_max([H|R], Min, Max) :-
    Min #= min(H, RMin),
    Max #= max(H, RMax),
    min_and_max(R, RMin, RMax).

part2(Out) :-
    puzzle_input(Inp), 
    numbers_invalid(Inp, Y), 
    numbers_sum_infix(Inp, Y, Infix), 
    min_and_max(Infix, Min, Max), 
    Out #= Min + Max.