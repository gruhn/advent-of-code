:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).

range(N, N, []).
range(I, N, [I|Is]) :-
    I #< N, J #= I + 1,
    range(J, N, Is).

zip([], [], []).
zip([A|As], [B|Bs], [(A,B)|ABs]) :-
    zip(As, Bs, ABs).

list_indexed(Ls, Is) :-
    length(Ls, N), 
    range(0, N, Rs),
    zip(Rs, Ls, Is).

not_member(_, []).
not_member(X, [Y|Ys]) :-
    dif(X, Y),
    not_member(X, Ys).


op_(nop(I)) --> "nop ", integer(I).
op_(acc(I)) --> "acc ", integer(I).
op_(jmp(I)) --> "jmp ", integer(I).

ops([])       --> [].
ops([In|Ins]) --> op_(In), eol, ops(Ins).

input(Ops) :-
    phrase_from_file(ops(Ops0), "input/08.txt"),
    list_indexed(Ops0, Ops).

% ops_linear(Line, Ops0) --> { zip(Ls, _, Ops0), not_member(Line, Ls) }.
% ops_linear(Line, Ops0) --> { from_op_to(Line, Op, Next), select((Line, Op), Ops0, Ops1) },
%    [ (Line, Op) ], ops_linear(Next, Ops1). 

op_apply(acc(X), From, To, Acc0, Acc) :-
    To #= From + 1,
    Acc #= Acc0 + X.
op_apply(nop(_), From, To, Acc, Acc) :-
    To #= From + 1.
op_apply(jmp(Jump), From, To, Acc, Acc) :-
    To #= From + Jump.

ops_converge(Ops) :-
    length(Ops, End),
    ops_converge(Ops, 0, End).
ops_converge(Ops0, From, End) :-
    op_apply(Op, From, End, _, _),
    select((From,Op), Ops0, _).
ops_converge(Ops0, From, End) :-
    dif(To, End),
    op_apply(Op, From, To, _, _), 
    select((From,Op), Ops0, Ops1),
    ops_converge(Ops1, To, End).

ops_accum(Ops, Acc) :-
    ops_accum(Ops, 0, Acc).
ops_accum(Ops0, From, 0) :-
    zip(Lines, _, Ops0), 
    not_member(From, Lines).
ops_accum(Ops0, From, Acc) :-
    op_apply(Op, From, To, Acc0, Acc),
    select((From, Op), Ops0, Ops1),
    ops_accum(Ops1, To, Acc0).

ops_swap_ops(Ops0, Functor0, Functor, [ (I,Op) | Ops ]) :-
    Op0 =.. [Functor0, Arg],
    select((I,Op0), Ops0, Ops),
    Op =.. [Functor, Arg].

part1(Acc) :-
    input(Ops0),
    ops_accum(Ops0, Acc).

part2(Acc) :-
    input(Ops0),
    ops_swap_ops(Ops0, jmp, nop, Ops),
    ops_converge(Ops),
    ops_accum(Ops, Acc).
part2(Acc) :-
    input(Ops0),
    ops_swap_ops(Ops0, nop, jmp, Ops),
    ops_converge(Ops),
    ops_accum(Ops, Acc).