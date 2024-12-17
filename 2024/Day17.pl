:- set_prolog_flag(double_quotes, codes).
:- use_module(library(clpfd)).
:- use_module(library(time)).
:- use_module(library(dcg/basics)).

% --------------------------------------------
% Solution
% --------------------------------------------

part1(Output) :-
  initial_state(State0),
  phrase((run, get(output(RevOutput))), State0, _),
  reverse(RevOutput, Output).

part2(RegA) :-
  initial_state(State0),
  phrase((
    set(regA(RegA)),
    run,
    get(output(RevOutput)),
    get(program(Program))
  ), State0, _),
  reverse(Program, RevOutput),
  labeling([bisect], [RegA]).

% --------------------------------------------
% Parser
% --------------------------------------------

integer_list([D|Ds]) --> integer(D), ",", !, integer_list(Ds).
integer_list([D])    --> integer(D).

input(RegA, RegB, RegC, Program) --> 
  "Register A: ", integer(RegA), "\n",
  "Register B: ", integer(RegB), "\n",
  "Register C: ", integer(RegC), "\n",
  "\n",
  "Program: ", integer_list(Program).

initial_state([RegA, RegB, RegC, [], Program, 0]) :-
  phrase_from_file((
    input(RegA, RegB, RegC, Program), 
    remainder(_)
  ), "input/17.txt").

% --------------------------------------------
% Interpreter
% --------------------------------------------

set(regA(A)),     [A,B,C,O,I,P] --> [_,B,C,O,I,P].
set(regB(B)),     [A,B,C,O,I,P] --> [A,_,C,O,I,P].
set(regC(C)),     [A,B,C,O,I,P] --> [A,B,_,O,I,P].
set(output(O)),   [A,B,C,O,I,P] --> [A,B,C,_,I,P].
set(program(I)),  [A,B,C,O,I,P] --> [A,B,C,O,_,P].
set(pointer(P)),  [A,B,C,O,I,P] --> [A,B,C,O,I,_].

get(Field, State, State) :- set(Field, State, State).

literal_operand(Operand) -->
  get(program(Program)),
  get(pointer(Pointer)),
  { OperandPointer #= Pointer + 1
  , nth0(OperandPointer, Program, Operand) 
  }.

combo_operand(Oper) --> literal_operand(Oper), { Oper in 0 .. 3 }.
combo_operand(RegA) --> literal_operand(4), get(regA(RegA)).
combo_operand(RegB) --> literal_operand(5), get(regB(RegB)).
combo_operand(RegC) --> literal_operand(6), get(regC(RegC)).

inc_pointer -->
  get(pointer(Pointer)),
  { NewPointer #= Pointer + 2 },
  set(pointer(NewPointer)).

% The adv instruction (opcode 0) performs division. 
% The numerator is the value in the A register. 
% The denominator is found by raising 2 to the power of the instruction's combo operand. 
% (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.) 
% The result of the division operation is truncated to an integer and then written to the 
% A register.
step(0) -->
  combo_operand(ComboOp),
  get(regA(RegA)),
  { NewRegA #= RegA div (2^ComboOp) },
  set(regA(NewRegA)),
  inc_pointer.
% The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's 
% literal operand, then stores the result in register B.
step(1) -->
  get(regB(RegB)),
  literal_operand(LiteralOperand),
  { NewRegB #= RegB xor LiteralOperand },
  set(regB(NewRegB)),
  inc_pointer.
% The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby 
% keeping only its lowest 3 bits), then writes that value to the B register.
step(2) -->
  combo_operand(ComboOp),
  { NewRegB #= ComboOp mod 8 },
  set(regB(NewRegB)),
  inc_pointer.
% The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A 
% register is not zero, it jumps by setting the instruction pointer to the value of its 
% literal operand; if this instruction jumps, the instruction pointer is not increased by 2 
% after this instruction.
step(3) --> get(regA(0)), inc_pointer.
step(3) -->
  get(regA(RegA)),
  { RegA #\= 0 },
  literal_operand(LiteralOperand),
  set(pointer(LiteralOperand)).
% The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C, 
% then stores the result in register B. (For legacy reasons, this instruction reads an 
% operand but ignores it.)
step(4) -->
  get(regB(RegB)),
  get(regC(RegC)),
  { NewRegB #= RegB xor RegC },
  set(regB(NewRegB)),
  inc_pointer.
% The out instruction (opcode 5) calculates the value of its combo operand modulo 8, 
% then outputs that value. (If a program outputs multiple values, they are separated by 
% commas.)
step(5) -->
  combo_operand(ComboOp),
  { Out #= ComboOp mod 8 },
  get(output(Output)),
  set(output([Out|Output])),
  inc_pointer.
% The bdv instruction (opcode 6) works exactly like the adv instruction except that the 
% result is stored in the B register. (The numerator is still read from the A register.)
step(6) -->
  combo_operand(ComboOp),
  get(regA(RegA)),
  { NewRegB #= RegA div (2^ComboOp) },
  set(regB(NewRegB)),
  inc_pointer.
% The cdv instruction (opcode 7) works exactly like the adv instruction except that the 
% result is stored in the C register. (The numerator is still read from the A register.)
step(7) -->
  combo_operand(ComboOp),
  get(regA(RegA)),
  { NewRegC #= RegA div (2^ComboOp) },
  set(regC(NewRegC)),
  inc_pointer.

run --> 
  get(program(Program)),
  get(pointer(Pointer)),
  { nth0(Pointer, Program, Instr) },
  step(Instr),
  run.
run --> [].
