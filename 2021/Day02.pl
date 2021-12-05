run(Result) :-
	input(Codes), 
    phrase(lines(Moves), Codes), 
    move_sum(Moves, [X, Y, _]), 
    Result is X*Y.

move_add([X1, Aim1], [X2, Y2, Aim2], [X3, Y3, Aim3]) :-
    X3 is X1 + X2,
    Aim3 is Aim1 + Aim2,
    Y3 is Y2 + X1*Aim3.

move_sum(Moves, Sum) :-
    foldl(move_add, Moves, [0,0,0], Sum).

% parse input:

lines([L|Ls]) --> move(L), "\n", !, lines(Ls).
lines([]) --> [].

move([X,0]) --> "forward ", !, integer(X).
move([0,Aim]) --> "down ", !, integer(Aim).
move([0,Aim]) --> "up ", !, integer(A), { Aim is A * -1 }.

integer(I) --> digit(D0), digits(D), { number_codes(I, [D0|D]) }.

digits([D|T]) --> digit(D), !, digits(T). 
digits([]) --> [].

digit(D) --> [D], { code_type(D, digit) }.