:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(time)).
:- use_module(library(pio), [phrase_from_file/2, phrase_to_stream/2]).
:- use_module(library(charsio), [char_type/2]).

program(Program) :-
    phrase_from_file(instructions(Program), "input/24.txt").

part1(Chars) :- 
    program(Program),
    length(Input, 14),
    Input ins 1..9,

    run(Program, Input, [_,_,_,0]), 

    maximum(Input),
    maplist(digit_char, Input, Chars).

part2(Chars) :-
    program(Program),
    length(Input, 14),
    Input ins 1..9,

    run(Program, Input, [_,_,_,0]), 

    minimum(Input),
    maplist(digit_char, Input, Chars).

digit_char(Digit, Char) :- 
    number_chars(Digit, [Char]).

main :-
    time(( part1(P1),  part2(P2) )),
    phrase_to_stream((
        "\n",
        "Part 1: ", P1, "\n",
        "Part 2: ", P2, "\n"
    ), user_output),
    halt.

% --------------------------------------------
% Parser
% --------------------------------------------

digit(D) --> [D], { char_type(D, decimal_digit) }.

digits([D|Ds]) --> digit(D), digits(Ds). 
digits([])     --> [].

decimal(Dec) --> digit(D), digits(Ds), { number_chars(Dec, [D|Ds]) }.

integer(NegNum) --> "-", decimal(Num), { NegNum #= Num * (-1) }.
integer(PosNum) --> decimal(PosNum).

register(reg(Char)) --> [Char].

value_or_register(#Val)     --> integer(Val).
value_or_register(reg(Reg)) --> [Reg].

operator(add) --> "add".
operator(mul) --> "mul".
operator(mod) --> "mod".
operator(div) --> "div".
operator(eql) --> "eql".

instruction(inp-Target)       --> "inp ", register(Target).
instruction(Op-Target-Source) -->
    operator(Op), " ", register(Target), " ", value_or_register(Source).

instructions([In|Ins]) --> instruction(In), "\n", instructions(Ins).
instructions([In])     --> instruction(In).
instructions([])       --> [].

% --------------------------------------------
% Interpreter
% --------------------------------------------

store(reg(w), #W), [W,X,Y,Z] --> [_,X,Y,Z].
store(reg(x), #X), [W,X,Y,Z] --> [W,_,Y,Z].
store(reg(y), #Y), [W,X,Y,Z] --> [W,X,_,Z].
store(reg(z), #Z), [W,X,Y,Z] --> [W,X,Y,_].

load(#Val, #Val, State0, State0).
load(reg(Var), #Val, State0, State0) :-
    store(reg(Var), #Val, _, State0).

eval(add,A,B,C) :- C #= A + B.
eval(mul,A,B,C) :- C #= A * B. 
eval(mod,A,B,C) :- C #= A mod B. 
eval(div,A,B,C) :- C #= A div B.
eval(eql,A,B,C) :- C #<==> (A #= B).

run_([], _) --> [].
run_([ inp-Target | Ins ], [Var|Input]) -->
    store(Target, #Var),
    run_(Ins, Input).
run_([ Op-Target-Source | Ins ], Input) -->
    load(Target, A),
    load(Source, B),
    { eval(Op, A, B, Result) },
    store(Target, #Result),
    run_(Ins, Input).

run(Program, Input, Output) :-
    phrase(run_(Program, Input), [0,0,0,0], Output).

% --------------------------------------------
% Labeling Utilities
% --------------------------------------------

wrap(Functor, Argument, Wrapped) :-
    Wrapped =.. [Functor, Argument].

maximum(Vars) :-
    maplist(wrap(max), Vars, VarsLabeled),
    labeling(VarsLabeled, Vars).

minimum(Vars) :-
    maplist(wrap(min), Vars, VarsLabeled),
    labeling(VarsLabeled, Vars).
