:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(pio), [phrase_from_file/2]).
:- use_module(library(charsio), [char_type/2]).

digits_([D|T]) --> digit_(D), !, digits_(T). 
digits_([]) --> [].

digit_(D) --> [D], { char_type(D, decimal_digit) }.

uint_(Int) --> digit_(D), digits_(Ds), { number_chars(Int, [D|Ds]) }.

int_(NegNum) --> "-", !, uint_(Num), { NegNum #= Num * (-1) }.
int_(PosNum) --> uint_(PosNum).

register_(Char) --> [Char].

value_(#Val) --> int_(Val), !.
value_(Var) --> register_(Var).

op_((inp, Reg))      --> "inp ", !, register_(Reg).
op_((add, Reg, Val)) --> "add ", !, register_(Reg), " ", value_(Val).
op_((mul, Reg, Val)) --> "mul ", !, register_(Reg), " ", value_(Val).
op_(((mod), Reg, Val)) --> "mod ", !, register_(Reg), " ", value_(Val).
op_(((div), Reg, Val)) --> "div ", !, register_(Reg), " ", value_(Val).
op_((eql, Reg, Val)) --> "eql ", !, register_(Reg), " ", value_(Val).

lines_([L|Ls]) --> op_(L), "\n", !, lines_(Ls).
lines_([L])    --> op_(L), !.
lines_([])     --> [].

program(Ops) :-
    phrase_from_file(lines_(Ops), "24-input.txt").

register_state_value(w, (W,_,_,_), #W).
register_state_value(x, (_,X,_,_), #X).
register_state_value(y, (_,_,Y,_), #Y).
register_state_value(z, (_,_,_,Z), #Z).
register_state_value(#Val, _, #Val). 

register_value_state0_state(w, W, (_,X,Y,Z), (W,X,Y,Z)).
register_value_state0_state(x, X, (W,_,Y,Z), (W,X,Y,Z)).
register_value_state0_state(y, Y, (W,X,_,Z), (W,X,Y,Z)).
register_value_state0_state(z, Z, (W,X,Y,_), (W,X,Y,Z)).

op_calc(add,A,B,C) :- C #= A + B.
op_calc(mul,A,B,C) :- C #= A * B. 
op_calc((mod),A,B,C) :- C #= A mod B. 
op_calc((div),A,B,C) :- C #= A div B.
op_calc(eql,A,B,C) :- C #<==> (A #= B).

program_input_state0_state([], _, StateN, StateN).
program_input_state0_state([(inp, Reg)|Ops], [Var|Input], State0, StateN) :-
    register_value_state0_state(Reg, Var, State0, State1),
    program_input_state0_state(Ops, Input, State1, StateN).
program_input_state0_state([(Op, RegA, RegB)|Ops], Input, State0, StateN) :-
    register_state_value(RegA, State0, A),
    register_state_value(RegB, State0, B),
    op_calc(Op, A, B, Out),
    register_value_state0_state(RegA, Out, State0, State1),
    program_input_state0_state(Ops, Input, State1, StateN).

run(Input, (W,X,Y,Z)) :-
    length(Input, 14),
    Input ins 1..9,
    program(Ps), 
    program_input_state0_state(Ps, Input, (0,0,0,0), (W,X,Y,Z)).

lift(F, V, Lifted) :-
    Lifted =.. [F, V].

maximum(Vars) :-
    maplist(lift(max), Vars, VarsLabeled),
    labeling(VarsLabeled, Vars).

minimum(Vars) :-
    maplist(lift(min), Vars, VarsLabeled),
    labeling(VarsLabeled, Vars).

part1(Input) :-
    run(Input, (_,_,_,0)),
    maximum(Input).

part2(Input) :-
    run(Input, (_,_,_,0)),
    minimum(Input).