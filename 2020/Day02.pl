:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).

line((A, B, Char, Pass)) --> 
    integer(A), "-", integer(B), " ", nonblank(CharCode), ": ", string_without("\n", PassCodes),
    { atom_codes(Char, [CharCode]), atom_codes(Pass, PassCodes) }.

lines([]) --> eos.
lines([L|Ls]) -->
    line(L), eol, lines(Ls).

length_valid_password((Min, Max, Char, Pass)) :-
    atom_chars(Pass, Chars),
    include(==(Char), Chars, Filtered),
    length(Filtered, L),
    Min #=< L, L #=< Max.

match_valid_password((A, B, Char, Pass)) :-
    atom_chars(Pass, Chars),
    nth1(A, Chars, CharA),
    nth1(B, Chars, CharB),
    member(Char, [CharA, CharB]),
    dif(CharA, CharB).

valid_password_count(N1, N2) :-
    phrase_from_file(lines(Lines), "input/02.txt"),
    include(length_valid_password, Lines, LengthValidLines),
    length(LengthValidLines, N1),
    include(match_valid_password, Lines, MatchValidLines),
    length(MatchValidLines, N2).