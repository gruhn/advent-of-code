:- module(parse, 
    [ whitespace/0
    , newline//0
    , string//1
    , letter//1
    , digit//1
    , many//2
    , many//1
    , many1//2
    , many1//1
    , sep_by//3
    , natural//1
    , word//1 ]).

:- use_module(library(dcgs)).
:- use_module(library(charsio), [char_type/2]).

whitespace --> " ". % [C], { char_type(C, whitespace), dif(C, '\n') }.

newline --> "\n".

string(S) --> S.

letter(C) --> [C], { char_type(C, alpha) }.

digit(C) --> [C], { char_type(C, decimal_digit) }.

natural(N) --> many1(digit, Chars), { number_chars(N, Chars) }.

word(W) --> many1(letter, Chars), { atom_chars(W, Chars) }.

% ---

:- meta_predicate many(1, ?, ?, ?).
:- meta_predicate many(0, ?, ?).

many(Rule, [X|Xs]) --> call(Rule, X), !, many(Rule, Xs).
many(_, [])        --> [].

many(Rule) --> call(Rule), !, many(Rule).
many(_)    --> [].

:- meta_predicate many1(1, ?, ?, ?).
:- meta_predicate many1(0, ?, ?).

many1(Rule, [X|Xs]) --> call(Rule, X), many(Rule, Xs).

many1(Rule) --> call(Rule), many(Rule).

:- meta_predicate sep_by(1, ?, ?, ?, ?).

sep_by(Item, Sep, [X|Xs]) --> call(Item, X), Sep, !, sep_by(Item, Sep, Xs).
sep_by(Item, _, [X])      --> call(Item, X).
