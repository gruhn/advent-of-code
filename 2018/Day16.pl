:- use_module(library(dcgs)).
:- use_module(library(clpz)).
:- use_module(library(si)).
:- use_module(library(charsio), [char_type/2]).
:- use_module(library(pio), [phrase_from_file/2]).
:- use_module(library(dif), [dif/2]).
:- use_module(library(reif)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(debug)).
:- use_module(library(lambda)).
:- use_module(library(assoc), [list_to_assoc/2]).

whitespace --> " ". % [C], { char_type(C, whitespace), dif(C, '\n') }.

newline --> "\n".

string(S) --> S.

letter(C)  --> [C], { char_type(C, alpha) }.

digit(C) --> [C], { char_type(C, decimal_digit) }.

% ---

many(Rule, [X|Xs]) --> call(Rule, X), !, many(Rule, Xs).
many(_, [])        --> [].

many(Rule) --> call(Rule), !, many(Rule).
many(_)    --> [].

many1(Rule, [X|Xs]) --> call(Rule, X), many(Rule, Xs).

many1(Rule) --> call(Rule), many(Rule).

sep_by(Item, Sep, [X|Xs]) --> call(Item, X), Sep, !, sep_by(Item, Sep, Xs).
sep_by(Item, _, [X])      --> call(Item, X).

natural(N) --> many1(digit, Chars), { number_chars(N, Chars) }.

% ---

instruction(OpCode-Args) -->
    sep_by(natural, whitespace, [OpCode|Args]).

sample(OpCode-[Args, Before, After]) --> 
    "Before: [", sep_by(natural, ", ", Before), "]", newline,
    instruction(OpCode-Args), newline,
    "After:  [", sep_by(natural, ", ", After), "]", newline.

% ---

replace(I0, [X|Xs], Y, Ys) :-
    if_( 0 #< I0
        ,   ( I #= I0 - 1
            , replace(I, Xs, Y, Yss)
            , Ys = [X|Yss]
            )
        , Ys = [Y|Xs]
        ).

% addr (add register) stores into register C the result of adding register A and register B.
op_args_state0_state(addr, [A,B,C], State0, State) :-
    nth0(A, State0, ValA),
    nth0(B, State0, ValB),
    ValC #= ValA + ValB,
    replace(C, State0, ValC, State).
% addi (add immediate) stores into register C the result of adding register A and value B.
op_args_state0_state(addi, [A,B,C], State0, State) :-
    nth0(A, State0, ValA),
    ValC #= ValA + B,
    replace(C, State0, ValC, State).
% mulr (multiply register) stores into register C the result of multiplying register A and register B.
op_args_state0_state(mulr, [A,B,C], State0, State) :-
    nth0(A, State0, ValA),
    nth0(B, State0, ValB),
    ValC #= ValA * ValB,
    replace(C, State0, ValC, State).
% muli (multiply immediate) stores into register C the result of multiplying register A and value B.
op_args_state0_state(muli, [A,B,C], State0, State) :-
    nth0(A, State0, ValA),
    ValC #= ValA * B,
    replace(C, State0, ValC, State).
% banr (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
op_args_state0_state(banr, [A,B,C], State0, State) :-
    nth0(A, State0, ValA),
    nth0(B, State0, ValB),
    ValC #= ValA /\ ValB,
    replace(C, State0, ValC, State).
% bani (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.
op_args_state0_state(bani, [A,B,C], State0, State) :-
    nth0(A, State0, ValA),
    ValC #= ValA /\ B,
    replace(C, State0, ValC, State).
% borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.
op_args_state0_state(borr, [A,B,C], State0, State) :-
    nth0(A, State0, ValA),
    nth0(B, State0, ValB),
    ValC #= ValA \/ ValB,
    replace(C, State0, ValC, State).
% bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.
op_args_state0_state(bori, [A,B,C], State0, State) :-
    nth0(A, State0, ValA),
    ValC #= ValA \/ B,
    replace(C, State0, ValC, State).
% setr (set register) copies the contents of register A into register C. (Input B is ignored.)
op_args_state0_state(setr, [A,_,C], State0, State) :-
    nth0(A, State0, ValA),
    replace(C, State0, ValA, State).
% seti (set immediate) stores value A into register C. (Input B is ignored.)
op_args_state0_state(seti, [A,_,C], State0, State) :-
    replace(C, State0, A, State).
% gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
op_args_state0_state(gtir, [A,B,C], State0, State) :-
    nth0(B, State0, ValB),
    ValC #<==> A #> ValB,
    replace(C, State0, ValC, State).
% gtri (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
op_args_state0_state(gtri, [A,B,C], State0, State) :-
    nth0(A, State0, ValA),
    ValC #<==> ValA #> B,
    replace(C, State0, ValC, State).
% gtrr (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
op_args_state0_state(gtrr, [A,B,C], State0, State) :-
    nth0(A, State0, ValA),
    nth0(B, State0, ValB),
    ValC #<==> ValA #> ValB,
    replace(C, State0, ValC, State).
% eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
op_args_state0_state(eqir, [A,B,C], State0, State) :-
    nth0(B, State0, ValB),
    ValC #<==> A #= ValB,
    replace(C, State0, ValC, State).
% eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
op_args_state0_state(eqri, [A,B,C], State0, State) :-
    nth0(A, State0, ValA),
    ValC #<==> ValA #= B,
    replace(C, State0, ValC, State).
% eqrr (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.
op_args_state0_state(eqrr, [A,B,C], State0, State) :-
    nth0(A, State0, ValA),
    nth0(B, State0, ValB),
    ValC #<==> ValA #= ValB,
    replace(C, State0, ValC, State).

% part1(N) :-
%     phrase_from_file(puzzle_input(Input0), "/mnt/2018/input/16.txt"),

%     transpose(Input0, [BeforeList, InstrList, AfterList]),
%     transpose(InstrList, [OpCodes|ArgCols]),
%     transpose(ArgCols, ArgList),

%     maplist(ops_args_state0_state, OpsList, ArgList, BeforeList, AfterList),
%     maplist(length, OpsList, OpsLen),
%     tfilter(#<(2), OpsLen, OpsLen1),
%     length(OpsLen1, N).

ops([ addr, addi, mulr, muli
    , banr, bani, borr, bori
    , setr, seti, gtir, gtri
    , gtrr, eqir, eqri, eqrr ]).

op_index(Op, Index) :-
    ops(Ops), 
    nth0(Index, Ops, Op).

op_samples(Op, Samples) :-
    transpose(Samples, [ArgList, BeforeList, AfterList]),
    maplist(op_args_state0_state(Op), ArgList, BeforeList, AfterList).

ops_samples(Ops, Samples) :-
    findall(Op, op_samples(Op, Samples), Ops).

in_list(Var, [X|Xs]) :-
    foldl(\A^B^C^(C=A\/B), Xs, X, Dom),
    Var in Dom.

samples_op_assoc(Samples0, OpAssoc) :-
    keysort(Samples0, Samples1),
    group_pairs_by_key(Samples1, SamplesGrouped),
    pairs_keys_values(SamplesGrouped, OpCodes, SampleGroups),

    maplist(ops_samples, OpsList, SampleGroups),
    maplist(maplist(op_index), OpsList, OpsIndexList),

    maplist(in_list, OpIndices, OpsIndexList),
    all_distinct(OpIndices),
    maplist(op_index, Ops, OpIndices),

    pairs_keys_values(Pairs, OpCodes, Ops),
    list_to_assoc(Pairs, OpAssoc).

part2(FinalState) :-
    phrase_from_file(sep_by(sample, newline, Samples0), "/mnt/2018/input/16_samples.txt"),
    samples_op_assoc(Samples0, OpAssoc),

    phrase_from_file(sep_by(instruction, newline, Program0), "/mnt/2018/input/16_program.txt"),
    pairs_keys_values(Program0, OpCodes, ArgsList),
    maplist(\OpCode^Op^get_assoc(OpCode, OpAssoc, Op), OpCodes, Ops),

    foldl(op_args_state0_state, Ops, ArgsList, [0,0,0,0], FinalState).