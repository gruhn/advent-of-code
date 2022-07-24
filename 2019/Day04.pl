:- use_module(library(clpz)).
:- use_module(library(lists), [ length/2, maplist/3, member/2 ]).
:- use_module(library(dif), [ dif/2 ]).

% puzzle input:
lower_bound(193651).
upper_bound(649729).

stutter([X,X|Xs]).
stutter([X,Y|Xs]) :-
    dif(X, Y),
    stutter([Y|Xs]).

number_digits(0, []).
number_digits(N, [D|Ds]) :-
    D in 0..9,
    length(Ds, Exp),
    N #= D * 10^Exp + N0,
    number_digits(N0, Ds).

password_part1(Digits) :-
    length(Digits, 6),
    Digits ins 0..9,
    chain(#=<, Digits), 
    stutter(Digits),

    number_digits(Num, Digits),
    lower_bound(LowerBound),
    LowerBound #=< Num, 
    upper_bound(UpperBound),
    Num #=< UpperBound,

    label(Digits).

list_groups([], []).
list_groups([X], [[X]]).
list_groups([Y,Y|Xs], [[Y|Ys]|Gs]) :-
    list_groups([Y|Xs], [Ys|Gs]).
list_groups([X,Y|Xs], [[X],Ys|Gs]) :-
    dif(X,Y),
    list_groups([Y|Xs], [Ys|Gs]).

password_part2(LowerBound, UpperBound, Digits) :-
    password_part1(LowerBound, UpperBound, Digits),
    list_groups(Digits, Runs),
    maplist(length, Runs, RunLengths),
    member(2, RunLengths).

countall(Temp, Goal, Count) :-
    setof(Temp, Goal, Set),
    length(Set, Count).

part1(N) :-
    countall(Xs, password_part1(Xs), N).

part2(N) :-
    countall(Xs, password_part2(Xs), N).