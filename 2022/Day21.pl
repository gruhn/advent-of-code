:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(pio), [ phrase_from_file/2 ]).
:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(pairs)).
:- use_module(utils, [ integer//1, word//1 ]).

part1(Root) :-
  input(VarDict, RootExpr, HumnExpr),
  Root #= RootExpr,
  Humn #= HumnExpr,
  vars_var_value(VarDict, root, Root),
  vars_var_value(VarDict, humn, Humn).

part2(Humn) :-
  input(VarDict, RootExpr, _),
  RootExpr =.. [_, Var1, Var2],
  Var1 #= Var2,
  vars_var_value(VarDict, humn, Humn).

% Evalution

input(VarDict, RootExpr, HumnExpr) :-
  phrase_from_file(equations(Eqs0), "input/21.txt"),

  pairs_keys(Eqs0, VarNames),
  pairs_keys(VarPairs, VarNames),
  list_to_assoc(VarPairs, VarDict),

  select(root-RootRHS, Eqs0, Eqs1),
  select(humn-HumnRHS, Eqs1, Eqs2),
  vars_rhs_expr(VarDict, RootRHS, RootExpr),
  vars_rhs_expr(VarDict, HumnRHS, HumnExpr),

  maplist(vars_equation_expr(VarDict), Eqs2, Eqs),
  maplist(call, Eqs).

vars_var_value(VarDict, VarName, Value) :-
  get_assoc(VarName, VarDict, Value).

vars_rhs_expr(_, #Int, #Int).
vars_rhs_expr(VarDict, [Op, VarName1, VarName2], Expr) :-
  vars_var_value(VarDict, VarName1, Value1),
  vars_var_value(VarDict, VarName2, Value2),
  Expr =.. [Op, Value1, Value2].

vars_equation_expr(VarDict, VarName-RHS, Value #= Expr) :-
  vars_var_value(VarDict, VarName, Value),
  vars_rhs_expr(VarDict, RHS, Expr).

% Parsing

equations([E|Es]) --> equation(E), "\n", equations(Es).
equations([E])    --> equation(E).

variable(Name) --> word(Chars), { atom_chars(Name, Chars) }.

equation(Var-RHS) --> variable(Var), ": ", equation_rhs(RHS).

equation_rhs(#Int)         --> integer(Int).
equation_rhs([Op, V1, V2]) --> variable(V1), " ", [Op], " ", variable(V2).