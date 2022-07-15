:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(pio), [phrase_from_file/2]).
:- use_module(library(charsio), [char_type/2]).

digits_([D|T]) --> digit_(D), !, digits_(T). 
digits_([])    --> [].

digit_(D) --> [D], { char_type(D, decimal_digit) }.

uint_(Int) --> digit_(D), digits_(Ds), { number_chars(Int, [D|Ds]) }.

int_(NegNum) --> "-", !, uint_(Num), { NegNum #= Num * (-1) }.
int_(PosNum) --> uint_(PosNum).

register_(Char) --> [Char].

value_(#Val) --> int_(Val), !.
value_(Var)  --> register_(Var).

op_((inp, Reg))        --> "inp ", !, register_(Reg).
op_((add, Reg, Val))   --> "add ", !, register_(Reg), " ", value_(Val).
op_((mul, Reg, Val))   --> "mul ", !, register_(Reg), " ", value_(Val).
op_(((mod), Reg, Val)) --> "mod ", !, register_(Reg), " ", value_(Val).
op_(((div), Reg, Val)) --> "div ", !, register_(Reg), " ", value_(Val).
op_((eql, Reg, Val))   --> "eql ", !, register_(Reg), " ", value_(Val).

lines_([L|Ls]) --> op_(L), "\n", !, lines_(Ls).
lines_([L])    --> op_(L), !.
lines_([])     --> [].

store(w, #W, (_,X,Y,Z), (W,X,Y,Z)).
store(x, #X, (W,_,Y,Z), (W,X,Y,Z)).
store(y, #Y, (W,X,_,Z), (W,X,Y,Z)).
store(z, #Z, (W,X,Y,_), (W,X,Y,Z)).

load(#Val, _, #Val).
load(Var, State, #Val) :-
    store(Var, #Val, _, State).

op_calc((add),A,B,C) :- C #= A + B.
op_calc((mul),A,B,C) :- C #= A * B. 
op_calc((mod),A,B,C) :- C #= A mod B. 
op_calc((div),A,B,C) :- C #= A div B.
op_calc((eql),A,B,C) :- C #<==> (A #= B).

run_([], _, StateN, StateN).
run_([(inp, Reg)|Ops], [Var|Input], State0, StateN) :-
    store(Reg, #Var, State0, State1),
    run_(Ops, Input, State1, StateN).
run_([(Op, RegA, RegB)|Ops], Input, State0, StateN) :-
    load(RegA, State0, A),
    load(RegB, State0, B),
    op_calc(Op, A, B, Out),
    store(RegA, #Out, State0, State1),
    run_(Ops, Input, State1, StateN).

run(Input) :-
    phrase_from_file(lines_(Ops), "24-input.txt"),
    length(Input, 14),
    Input ins 1..9,
    run_(Ops, Input, (0,0,0,0), (_,_,_,0)).

lift(F, V, Lifted) :-
    Lifted =.. [F, V].

maximum(Vars) :-
    maplist(lift(max), Vars, VarsLabeled),
    labeling(VarsLabeled, Vars).

minimum(Vars) :-
    maplist(lift(min), Vars, VarsLabeled),
    labeling(VarsLabeled, Vars).

part1(Input) :- 
    run(Input), maximum(Input).

part2(Input) :- 
    run(Input), minimum(Input).