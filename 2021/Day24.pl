:- use_module(library(clpfd)).

digits([D|T]) --> digit(D), !, digits(T). 
digits([]) --> [].

digit(D) --> [D], { code_type(D, digit) }.

uint(Int) --> digit(D), digits(Ds), { number_codes(Int, [D|Ds]) }.

int(NegNum) --> "-", !, uint(Num), { NegNum #= Num * (-1) }.
int(PosNum) --> uint(PosNum).

register(Var) --> [Char], !, { atom_codes(Var, [Char]) }.

value(Val) --> int(Val), !.
value(Var) --> register(Var).

instr((inp, Reg))      --> "inp ", !, register(Reg).
instr((add, Reg, Val)) --> "add ", !, register(Reg), " ", value(Val).
instr((mul, Reg, Val)) --> "mul ", !, register(Reg), " ", value(Val).
instr((mod, Reg, Val)) --> "mod ", !, register(Reg), " ", value(Val).
instr((div, Reg, Val)) --> "div ", !, register(Reg), " ", value(Val).
instr((eql, Reg, Val)) --> "eql ", !, register(Reg), " ", value(Val).

lines([L|Ls]) --> instr(L), "\n", !, lines(Ls).
lines([L])    --> instr(L), !.
lines([])     --> [].

instructions(Instr) :-
    phrase_from_file(lines(Instr), "24-input.txt").

load(Val, _, Val) :- integer(Val).
load(w, (W,_,_,_), W).
load(x, (_,X,_,_), X).
load(y, (_,_,Y,_), Y).
load(z, (_,_,_,Z), Z).

store(w, W, (_,X,Y,Z), (W,X,Y,Z)).
store(x, X, (W,_,Y,Z), (W,X,Y,Z)).
store(y, Y, (W,X,_,Z), (W,X,Y,Z)).
store(z, Z, (W,X,Y,_), (W,X,Y,Z)).

compute([], _, StateN, StateN).
compute([(inp, Reg)|Ops], [Var|Input], State0, StateN) :-
    store(Reg, Var, State0, State1),
    compute(Ops, Input, State1, StateN).
compute([(add, RegA, RegB)|Ops], Input, State0, StateN) :-
    load(RegA, State0, A),
    load(RegB, State0, B),
    Out #= A + B,
    store(RegA, Out, State0, State1),
    compute(Ops, Input, State1, StateN).
compute([(mul, RegA, RegB)|Ops], Input, State0, StateN) :-
    load(RegA, State0, A),
    load(RegB, State0, B),
    Out #= A * B,
    store(RegA, Out, State0, State1),
    compute(Ops, Input, State1, StateN).
compute([(div, RegA, RegB)|Ops], Input, State0, StateN) :-
    load(RegA, State0, A),
    load(RegB, State0, B),
    Out #= A div B,
    store(RegA, Out, State0, State1),
    compute(Ops, Input, State1, StateN).
compute([(mod, RegA, RegB)|Ops], Input, State0, StateN) :-
    load(RegA, State0, A),
    load(RegB, State0, B),
    Out #= A mod B,
    store(RegA, Out, State0, State1),
    compute(Ops, Input, State1, StateN).
compute([(eql, RegA, RegB)|Ops], Input, State0, StateN) :-
    load(RegA, State0, A),
    load(RegB, State0, B),
    A #= B,
    store(RegA, 1, State0, State1),
    compute(Ops, Input, State1, StateN).
compute([(eql, RegA, RegB)|Ops], Input, State0, StateN) :-
    load(RegA, State0, A),
    load(RegB, State0, B),
    A #\= B,
    store(RegA, 0, State0, State1),
    compute(Ops, Input, State1, StateN).

label_max(X, max(X)).

label_min(X, min(X)).

maximum(Vars) :-
    maplist(label_max, Vars, VarsLabeled),
    labeling(VarsLabeled, Vars).

minimum(Vars) :-
    maplist(label_min, Vars, VarsLabeled),
    labeling(VarsLabeled, Vars).

part1(Input) :-
    length(Input, 14),
    Input ins 1..9,
    instructions(Ps), compute(Ps, Input, (0,0,0,0), (_,_,_,0)),
    maximum(Input).

part2(Input) :-
    length(Input, 14),
    Input ins 1..9,
    instructions(Ps), compute(Ps, Input, (0,0,0,0), (_,_,_,0)),
    minimum(Input).
