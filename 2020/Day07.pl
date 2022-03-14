:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

lexeme(Atom) --> string_without(" ,.\n", Codes), blanks,
	{ atom_codes(Atom, Codes) }.

bag(bag(Attr, Color)) --> 
    lexeme(Attr), lexeme(Color), lexeme("bags").
bag(bag(Attr, Color)) -->
    lexeme(Attr), lexeme(Color), lexeme("bag").

bag_count([Bag, Count]) --> lexeme(CountAtom), bag(Bag),
    { atom_number(CountAtom, Count) }.

bags([]) 	 --> "no other bags".
bags([B|Bs]) --> bag_count(B), ", ", bags(Bs).
bags([B]) 	 --> bag_count(B).

contain_rule(Parent-Children) --> 
    bag(Parent), lexeme("contain"), bags(Children), ".".

contain_rules([R|Rs]) --> 
    contain_rule(R), eol, contain_rules(Rs).
contain_rules([]) --> eos.

container_bags(Rules, BagSet) :-
    pairs_keys(Rules, Bags),
    list_to_set(Bags, BagSet).

:- table contains/3.
contains(Rules, ParentBag, ChildBag) :-
    member(ParentBag-Children, Rules),
    member([ChildBag, _], Children).
contains(Rules, ParentBag, GrandChildBag) :-
    member(ParentBag-Children, Rules),
    member([ChildBag, _], Children),
    contains(Rules, ChildBag, GrandChildBag).

part1(N) :-
    phrase_from_file(contain_rules(Rules), "input/07.txt"),
	findall(Parent, contains(Rules, Parent, bag(shiny, gold)), List),
	length(List, N).

:- table contain_count/3.
contain_count(Rules, [Bag,SelfCount], Count) :-
	member(Bag-Children, Rules),
	maplist(contain_count(Rules), Children, Counts),
	sum(Counts, #=, Sum),
    Count #= Sum * SelfCount + SelfCount.

part2(N) :-
    phrase_from_file(contain_rules(Rules), "input/07.txt"),
    contain_count(Rules, [bag(shiny, gold), 1], N0),
    N #= N0 - 1.