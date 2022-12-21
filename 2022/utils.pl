:- module(utils, [ integer//1, decimal//1, word//1 ]).

:- use_module(library(dcgs)).
:- use_module(library(clpz)).
:- use_module(library(charsio), [ char_type/2 ]).

char(Char, Type) --> [Char], { char_type(Char, Type) }.

digits([D|Ds]) --> char(D, decimal_digit), digits(Ds).
digits([D])    --> char(D, decimal_digit).

decimal(D) --> digits(Ds), { number_chars(D, Ds) }.

integer(Int) --> "-", decimal(Dec), { Int #= -1 * Dec }.
integer(Int) --> decimal(Int).

word([C|Cs]) --> char(C, alpha), word(Cs).
word([C])    --> char(C, alpha).