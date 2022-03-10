:- use_module(library(clpfd)).

seat([])     --> [].
seat([0|Bs]) --> "F", seat(Bs).
seat([1|Bs]) --> "B", seat(Bs).
seat([0|Bs]) --> "L", seat(Bs).
seat([1|Bs]) --> "R", seat(Bs).

seats([])     --> [].
seats([S])    --> seat(S).
seats([S|SS]) --> seat(S), "\n", seats(SS).

binary_decimal([], 0).
binary_decimal([B|Bs], D) :-
    B in 0 .. 1,
    length(Bs, Exp),
    D #= B * 2^Exp + D0,
    binary_decimal(Bs, D0).

seat_id(Seat, ID) :-
    binary_decimal(Seat, ID).

max(A, B, Max) :- Max #= max(A,B).

list_max(Ls, Max) :- 
    foldl(max, Ls, 0, Max).

enum_skip(Ls, Skip) :-
    sort(Ls, Enum),
    X + 2 #= Y,
    append(_, [X,Y|_], Enum),
    Skip #= X + 1.

solution(Part1, Part2) :-
    phrase_from_file(seats(SS0), "input/05.txt"),
    maplist(seat_id, SS0, SS),
    list_max(SS, Part1),
    enum_skip(SS, Part2).