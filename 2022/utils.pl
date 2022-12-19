:- module(utils, [ integer//1, decimal//1 ]).

:- use_module(library(dcgs)).
:- use_module(library(clpz)).
:- use_module(library(charsio), [ char_type/2 ]).

digit(D) --> [D], { char_type(D, decimal_digit) }.

digits([D|Ds]) --> digit(D), digits(Ds).
digits([D])    --> digit(D).

decimal(D) --> digits(Ds), { number_chars(D, Ds) }.

integer(Int) --> "-", decimal(Dec), { Int #= -1 * Dec }.
integer(Int) --> decimal(Int).