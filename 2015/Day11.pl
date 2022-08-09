:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(dcgs)).

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
    straight(Straight),
    phrase((...,seq(Straight),...), Codes1),

    % passwords must contain at least two different, non-overlapping pairs
    phrase((...,seq([X,X]),...,seq([Y,Y]),...), Codes).

wrap_min(X, min(X)).

solution(NextPassword) :-
    chars_codes("vzbxkghb", OldCodes),
    password(OldCodes, NextCodes),

    % FIXME: expected output is
    %
    %  ?- solution(Next).
    %  ;  Next = "vzbxxyzz"
    %  ;  Next = "vzcaabcc"
    %  ;  ...
    %
    % but we get 
    %
    %  ?- solution(Next).
    %  ;  Next = "vzbxxyzz"
    %  ;  Next = "vzzaaabc"
    %  ;  Next = "vzzaabcd" 
    %  ;  ...
    %
    % i.e. the enumeration jumps over some smaller
    % values. By manually constraining the result
    % with upper bounds we can find find the smallest 
    % successor and prove that it's the smallest but
    % would be nicer to make fix this.

    chars_codes("vzzaaabc", UpperBound1),
    after(NextCodes, UpperBound1),

    chars_codes("vzccaabc", UpperBound2),
    after(NextCodes, UpperBound2),

    maplist(wrap_min, NextCodes, Mins),
    labeling(Mins, NextCodes),
    chars_codes(NextPassword, NextCodes).
