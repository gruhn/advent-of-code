:- use_module(library(clpz)).
:- use_module(library(dcgs)).
:- use_module(library(pio), [ phrase_from_file/2 ]).
:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(pairs)).
:- use_module(utils, [ integer//1, word//1 ]).

part1(Root) :-
  input(EqDict),
  get_assoc(root, EqDict, Root #= _),
  map_assoc(call, EqDict).

part2(Humn) :-
  input(EqDict0),
  del_assoc(humn, EqDict0, Humn #= _, EqDict1),
  del_assoc(root, EqDict1, _ #= RootExpr, EqDict),
  RootExpr =.. [_, Value1, Value2],
  Value1 #= Value2,
  map_assoc(call, EqDict).

% Evaluation

input(EqDict) :-
  phrase_from_file(equations(VarEqPairs0), "input/21.txt"),

  pairs_keys_values(VarEqPairs0, Vars, Eqs0),
  pairs_keys(VarValuePairs, Vars),
  list_to_assoc(VarValuePairs, ValueDict),

  maplist(var_subst(ValueDict), Eqs0, Eqs),
  pairs_keys_values(VarEqPairs, Vars, Eqs),
  list_to_assoc(VarEqPairs, EqDict).

var_subst(ValueDict, Term0, Term) :-
  ( atom(Term0) -> get_assoc(Term0, ValueDict, Term) ) ; 
  Term0 =.. [ Functor | SubTerms0 ],
  maplist(var_subst(ValueDict), SubTerms0, SubTerms),
  Term =.. [ Functor | SubTerms ].

% Parsing

equations([E|Es]) --> equation(E), "\n", equations(Es).
equations([E])    --> equation(E).

variable(Name) --> word(Chars), { atom_chars(Name, Chars) }.

equation(Var-(Var #= RHS)) --> variable(Var), ": ", equation_rhs(RHS).

equation_rhs(#Int) --> integer(Int).
equation_rhs(Term) --> 
  variable(V1), " ", [Op], " ", variable(V2),
  { Term =.. [Op, V1, V2] }.