:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(library(time)).

not_ins(_, []).
not_ins(Xs, [Y|Ys]) :-
    maplist(#\=(Y), Xs),
    not_ins(Xs, Ys).

chars_codes(Chars, Codes) :-
    maplist(char_code, Chars, Codes).

char_code_domain(StartChar, EndChar, StartCode .. EndCode) :-
    char_code(StartChar, StartCode),
    char_code(EndChar, EndCode).

straight([]).
straight([_]).
straight([C1,C2|Cs]) :-
    C1 #= C2 + 1,
    straight([C2|Cs]).

after([C|Cs], [D|Ds]) :- 
    same_length(Cs, Ds), 
    C #< D.
after([C|Cs], [C|Ds]) :- 
    same_length(Cs, Ds), 
    after(Cs, Ds).

password(Codes) :-
    % passwords must be exactly eight lowercase letters
    length(Codes, 8),
    char_code_domain(a, z, A_to_Z),
    Codes ins A_to_Z,

    % passwords may not contain the letters i, o, or l
    chars_codes("iol", Excluded),
    not_ins(Codes, Excluded),
    reverse(Codes, CodesRev),

    % passwords must include one increasing straight of at least three letters
    length(Straight, 3),
    straight(Straight),
    phrase((...,seq(Straight),...), CodesRev),

    % passwords must contain at least two different, non-overlapping pairs
    phrase((...,seq([X,X]),...,seq([Y,Y]),...), CodesRev).

% FIXME: enumeration is a bit off. In the beginning solutions are repeated. Why?
password_between(Codes, LowerBound, UpperBound) :-
    password(Between),
    after(LowerBound, Between),
    after(Between, UpperBound),

    label(Between),

    ( password_between(Codes, LowerBound, Between) 
    ; Codes = Between
    ; password_between(Codes, Between, UpperBound)
    ).

solution(NextPassword) :-
    chars_codes("vzbxkghb", OldCodes),
    password_between(Codes, OldCodes, BoundCodes),
    chars_codes(NextPassword, Codes).