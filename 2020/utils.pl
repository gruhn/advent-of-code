:- module(utils, [ lexeme/3, uinteger/3, hex_number/3, alpha/3, char/3 ]).
:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

alpha([C|Cs]) --> [C], { char_type(C, alpha) }, alpha(Cs).
alpha([C])    --> [C], { char_type(C, alpha) }.

char(Char) --> [Char].

lexeme(Chars) --> string_without(" ,.\n", String), whites, 
    { string_chars(String, Chars), length(Chars, Len), Len #> 0 }.

uinteger(Num) --> digits(Chars), { number_chars(Num, Chars) }.

hex_digit(Atom) --> [H], 
    { atom_codes(Atom, [H]), member(Atom, [a,b,c,d,e,f,'0','1','2','3','4','5','6','7','8','9']) }.

hex_digits([H|Hs]) --> hex_digit(H), hex_digits(Hs).
hex_digits([]) --> [].

hex_number(Chars) --> hex_digits(Chars).