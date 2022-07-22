:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(pio), [phrase_from_file/2]).
:- use_module(library(charsio), [char_type/2]).

digits([D|Ds]) --> digit(D), digits(Ds). 
digits([])     --> [].

digit(D) --> [D], { char_type(D, decimal_digit) }.

integer(Int) --> digit(D), digits(Ds), { number_chars(Int, [D|Ds]) }.

signed_integer(NegNum) --> "-", integer(Num), { NegNum #= Num * (-1) }.
signed_integer(PosNum) --> integer(PosNum).

register(reg(Char)) --> [Char].

value(#Val)     --> signed_integer(Val).
value(reg(Var)) --> [Var].

binary_instruction(add)   --> "add".
binary_instruction(mul)   --> "mul".
binary_instruction((mod)) --> "mod".
binary_instruction((div)) --> "div".
binary_instruction(eql)   --> "eql".

instruction((inp, reg(Reg)))       --> "inp ", [Reg].
instruction((Inst, reg(Reg), Val)) -->
    binary_instruction(Inst), " ", [Reg], " ", value(Val).

instructions([L|Ls]) --> instruction(L), "\n", instructions(Ls).
instructions([L])    --> instruction(L).
instructions([])     --> [].

store(reg(w), #W, (_,X,Y,Z), (W,X,Y,Z)).
store(reg(x), #X, (W,_,Y,Z), (W,X,Y,Z)).
store(reg(y), #Y, (W,X,_,Z), (W,X,Y,Z)).
store(reg(z), #Z, (W,X,Y,_), (W,X,Y,Z)).

load(#Val, _, #Val).
load(reg(Var), State, #Val) :-
    store(reg(Var), #Val, _, State).

op_calc((add),A,B,C) :- C #= A + B.
op_calc((mul),A,B,C) :- C #= A * B. 
op_calc((mod),A,B,C) :- C #= A mod B. 
op_calc((div),A,B,C) :- C #= A div B.
op_calc((eql),A,B,C) :- C #<==> (A #= B).

state(S0, S), [S] --> [S0].

run_([], _) --> [].
run_([(inp, Reg)|Ops], [Var|Input]) -->
    state(State0, State1),
    { store(Reg, #Var, State0, State1) },
    run_(Ops, Input).
run_([(Op, RegA, RegB)|Ops], Input) -->
    state(State0, State1),
    { load(RegA, State0, A)
    , load(RegB, State0, B)
    , op_calc(Op, A, B, Out)
    , store(RegA, #Out, State0, State1) },
    run_(Ops, Input).

run(Input) :-
    once(phrase_from_file(instructions(Program), "24-input.txt")),
    length(Input, 14),
    Input ins 1..9,
    phrase(run_(Program, Input), [(0,0,0,0)], [(_,_,_,0)]).

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