:- use_module(library(clpz)).
:- use_module(library(lists), [length/2, maplist/2, maplist/3, same_length/2, reverse/2]).
:- use_module(library(dcgs)).
:- use_module(library(dif)).

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

list_infix(List, Infix) :-
    phrase((...,seq(Infix),...), List).

after([C|Cs], [C|Ds]) :- 
    same_length(Cs, Ds), 
    after(Cs, Ds).
after([C|Cs], [D|Ds]) :- 
    same_length(Cs, Ds), 
    C #< D.

password(OldCodes, Codes) :-
    % passwords must be exactly eight lowercase letters
    length(Codes, 8),
    char_code_domain(a, z, A_to_Z),
    Codes ins A_to_Z,

    % passwords may not contain the letters i, o, or l
    chars_codes("iol", Excluded),
    not_ins(Codes, Excluded),

    % finds new password by incrementing old password
    after(OldCodes, Codes),
    reverse(Codes, Codes1),

    % passwords must include one increasing straight of at least three letters
    length(Straight, 3),
    list_infix(Codes1, Straight),
    straight(Straight),

    % passwords must contain at least two different, non-overlapping pairs
    X #\= Y,
    list_infix(Codes1, [X,X]),
    list_infix(Codes1, [Y,Y]).

wrap_min(X, min(X)).

solution(NextPassword) :-
    chars_codes("vzbxkghb", OldCodes),
    password(OldCodes, NextCodes),

    % chars_codes("vzbxyyaa", MaxCodes),
    % after(NextCodes, MaxCodes),

    maplist(wrap_min, NextCodes, Mins),
    reverse(Mins, Mins1),
    labeling(Mins1, NextCodes),
    chars_codes(NextPassword, NextCodes).
