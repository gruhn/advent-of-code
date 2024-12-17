:- use_module(library(clpz)).
:- use_module(library(time)).
:- use_module(library(lists), [nth0/3, reverse/2, same_length/2, length/2]).

operand(state(_, _, _, _, Program, Pointer), Operand) :-
  Operand in 0 .. 7,
  OperandPointer #= Pointer + 1,
  length(Program, Len),
  OperandPointer in 0 .. Len,
  nth0(OperandPointer, Program, Operand).

combo_operand(State, Operand) :-
  Operand in 0 .. 3,
  operand(State, Operand).
combo_operand(State, RegA) :-
  operand(State, 4),
  regA(RegA, _, State, _).
combo_operand(State, RegB) :-
  operand(State, 5),
  regB(RegB, _, State, _).
combo_operand(State, RegC) :-
  operand(State, 6),
  regC(RegC, _, State, _).   

initial_state(RegA, RegB, RegC, Program, state(RegA, RegB, RegC, [], Program, 0)).

program(
  Program,
  state(_, _, _, _, Program, _)
).
regA(
  RegA,
  NewRegA,
  state(RegA, RegB, RegC, Output, Program, Pointer),
  state(NewRegA, RegB, RegC, Output, Program, Pointer)
) :- RegA #>= 0, NewRegA #>= 0.
regB(
  RegB,
  NewRegB,
  state(RegA, RegB, RegC, Output, Program, Pointer),
  state(RegA, NewRegB, RegC, Output, Program, Pointer)
) :- RegB #>= 0, NewRegB #>= 0.
regC(
  RegC,
  NewRegC,
  state(RegA, RegB, RegC, Output, Program, Pointer),
  state(RegA, RegB, NewRegC, Output, Program, Pointer)
) :- RegC #>= 0, NewRegC #>= 0.
pointer(
  Pointer,
  NewPointer,
  state(RegA, RegB, RegC, Output, Program, Pointer),
  state(RegA, RegB, RegC, Output, Program, NewPointer)   
).
output(
  Output,
  NewOutput,
  state(RegA, RegB, RegC, Output, Program, Pointer),
  state(RegA, RegB, RegC, NewOutput, Program, Pointer)   
).

inc_pointer(State0, State) :-
  pointer(Pointer, NewPointer, State0, State),
  program(Program, State0),
  length(Program, Len),
  NewPointer in 0 .. Len,
  NewPointer #= Pointer + 2.    

% The adv instruction (opcode 0) performs division. 
% The numerator is the value in the A register. 
% The denominator is found by raising 2 to the power of the instruction's combo operand. 
% (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.) 
% The result of the division operation is truncated to an integer and then written to the 
% A register.
step(0, State0, State) :-
  combo_operand(State0, ComboOp),
  regA(RegA, NewRegA, State0, State1),
  NewRegA #= RegA div (2^ComboOp),
  inc_pointer(State1, State).
% The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the instruction's 
% literal operand, then stores the result in register B.
step(1, State0, State) :-
  regB(RegB, NewRegB, State0, State1),
  operand(State0, LiteralOperand),
  NewRegB #= RegB xor LiteralOperand,
  inc_pointer(State1, State).
% The bst instruction (opcode 2) calculates the value of its combo operand modulo 8 (thereby 
% keeping only its lowest 3 bits), then writes that value to the B register.
step(2, State0, State) :-
  regB(_, NewRegB, State0, State1),
  combo_operand(State0, ComboOp),
  NewRegB #= ComboOp mod 8,
  inc_pointer(State1, State).
% The jnz instruction (opcode 3) does nothing if the A register is 0. However, if the A 
% register is not zero, it jumps by setting the instruction pointer to the value of its 
% literal operand; if this instruction jumps, the instruction pointer is not increased by 2 
% after this instruction.
step(3, State0, State) :-
  regA(0, _, State0, _),
  inc_pointer(State0, State).
step(3, State0, State) :-
  regA(RegA, _, State0, _),
  RegA #\= 0,
  operand(State0, LiteralOperand),
  pointer(_, LiteralOperand, State0, State).
% The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C, 
% then stores the result in register B. (For legacy reasons, this instruction reads an 
% operand but ignores it.)
step(4, State0, State) :-
  regB(RegB, NewRegB, State0, State1),
  regC(RegC, _      , State0, _     ),
  NewRegB #= RegB xor RegC,
  inc_pointer(State1, State).
% The out instruction (opcode 5) calculates the value of its combo operand modulo 8, 
% then outputs that value. (If a program outputs multiple values, they are separated by 
% commas.)
step(5, State0, State) :-
  combo_operand(State0, ComboOp),
  Res #= ComboOp mod 8,
  output(Output, [Res|Output], State0, State1),
  inc_pointer(State1, State).
% The bdv instruction (opcode 6) works exactly like the adv instruction except that the 
% result is stored in the B register. (The numerator is still read from the A register.)
step(6, State0, State) :-
  combo_operand(State0, ComboOp),
  regA(RegA, _, State0, _),
  NewRegB #= RegA div (2^ComboOp),
  regB(_, NewRegB, State0, State1),
  inc_pointer(State1, State).
% The cdv instruction (opcode 7) works exactly like the adv instruction except that the 
% result is stored in the C register. (The numerator is still read from the A register.)
step(7, State0, State) :-
  combo_operand(State0, ComboOp),
  regA(RegA, _, State0, _),
  NewRegC #= RegA div (2^ComboOp),
  regC(_, NewRegC, State0, State1),
  inc_pointer(State1, State).

run(State0, StateN) :-
  program(Program, State0),
  pointer(Pointer, _, State0, _),
  nth0(Pointer, Program, Instr),
  step(Instr, State0, State1),
  run(State1, StateN).
run(State, State).

 
% If register C contains 9, the program 2,6 would set register B to 1.
test1 :-
  initial_state(0, 0, 9, [2,6], State0),
  run(State0, StateN),
  regB(1, _, StateN, _).

% If register A contains 10, the program 5,0,5,1,5,4 would output 0,1,2.
test2 :-
  initial_state(10, 0, 0, [5,0,5,1,5,4], State0),
  run(State0, StateN),
  output(Output, _, StateN, _),
  reverse(Output, [0,1,2]).

% If register A contains 2024, the program 0,1,5,4,3,0 would output 4,2,5,6,7,7,7,7,3,1,0 and 
% leave 0 in register A.
test3 :- 
  initial_state(2024, 0, 0, [0,1,5,4,3,0], State0),
  run(State0, StateN),
  regA(0, _, StateN, _),
  output(Output, _, StateN, _),
  reverse(Output, [4,2,5,6,7,7,7,7,3,1,0]).

% If register B contains 29, the program 1,7 would set register B to 26.
test4 :- 
  initial_state(0, 29, 0, [1,7], State0),
  run(State0, StateN),
  regB(26, _, StateN, _).

% If register B contains 2024 and register C contains 43690, the program 4,0 would set 
% register B to 44354. 
test5 :-
  initial_state(0, 2024, 43690, [4,0], State0),
  run(State0, StateN),
  regB(44354, _, StateN, _).

test6(RegA) :- 
  Program = [0,3,5,4,3,0],
  initial_state(RegA, 0, 0, Program, State0),
  run(State0, StateN),
  reverse(Program, Output),
  output(Output, _, StateN, _),
  labeling([min(RegA)], [RegA]).

part1(RevOutput) :-
  initial_state(64196994, 0, 0, [2,4,1,1,7,5,1,5,4,0,0,3,5,5,3,0], State0),
  run(State0, StateN),
  output(Output, _, StateN, _),
  reverse(Output, RevOutput).

% 35184372088832 too low

part2(RegA) :-
  Program = [2,4,1,1,7,5,1,5,4,0,0,3,5,5,3,0],
  initial_state(RegA, 0, 0, [2,4,1,1,7,5,1,5,4,0,0,3,5,5,3,0], State0),
  RegA #>= 1,
  RegA #>= 35184372089164,
  run(State0, StateN),
  reverse(Program, Output),
  output(Output, _, StateN, _).
  % labeling([bisect], [RegA]).

stateN(StateN) :-
  Program = [2,4,1,1,7,5,1,5,4,0,0,3,5,5,3,0],
  reverse(Program, Output),
  length(Program, ProgramLength),
  Pointer #>= ProgramLength,
  StateN = state(_, _, _, Output, Program, Pointer).
