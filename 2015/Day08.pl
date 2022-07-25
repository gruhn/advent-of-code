:- use_module(library(dcgs)).
:- use_module(library(lists), [nth0/3, maplist/3, length/2]).
:- use_module(library(clpz)).
:- use_module(library(charsio), [char_type/2]).
:- use_module(library(dif), [dif/2]).
:- use_module(library(pio), [phrase_from_file/2]).

string_without([C|Cs], W) --> [C], { dif(C,W) }, string_without(Cs, W).
string_without([], _)     --> [].

lines([L|Ls]) --> string_without(L, '\n'), "\n", lines(Ls).
lines([])     --> "".

hex_digit(Hex, Digit) :-
    nth0(Digit, "0123456789abcdef", Hex).

chars_escaped(['\\'|Cs]) --> "\\\\", chars_escaped(Cs).
chars_escaped(['"'|Cs])  --> "\\\"", chars_escaped(Cs).
chars_escaped([C|Cs])    --> [C], chars_escaped(Cs).
chars_escaped([])        --> [].

string_escaped_(Chars) --> ['"'], chars_escaped(Chars), ['"'].

string_escaped(String, Escaped) :-
    phrase(string_escaped_(String), Escaped).

char_hex(Char) --> "\\x", [H1,H2], 
    { hex_digit(H1, D1)
    , hex_digit(H2, D2)
    , Code #= D1*16 + D2 
    , char_code(Char, Code)
    }.

chars_hex([C|Cs]) --> char_hex(C), chars_hex(Cs).
chars_hex([C|Cs]) --> [C], chars_hex(Cs).
chars_hex([])     --> [].

string_hex(String, HexChars) :-
    phrase(chars_hex(String), HexChars).

lists_total_length(Ls, Sum) :-
    maplist(length, Ls, Lengths),
    sum(Lengths, #=, Sum).

solution(Part1, Part2) :-
    phrase_from_file(lines(Escaped), "input/08.txt"),

    maplist(string_escaped, UnEscaped, Escaped),
    lists_total_length(Escaped, EscapedLength),

    maplist(string_hex, UnHexed, UnEscaped),
    lists_total_length(UnHexed, UnHexedLength),

    Part1 #= EscapedLength - UnHexedLength,

    maplist(string_escaped, Escaped, DoubleEscaped),
    lists_total_length(DoubleEscaped, DoubleEscapedLength),

    Part2 #= DoubleEscapedLength - EscapedLength.