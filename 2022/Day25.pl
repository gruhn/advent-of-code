:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(library(pio), [ phrase_from_file/2 ]).
:- use_module(library(charsio), [ char_type/2 ]).
:- use_module(library(dif)).

part1(SnafuStr) :-
  phrase_from_file(snafus(Snafus), "input/25.txt"),

  maplist(decimal_snafu, Decimals, Snafus), 
  sum_list(Decimals, Sum), 

  decimal_snafu(Sum, SnafuSum), 
  phrase(snafu(SnafuSum), SnafuStr).

decimal_snafu(0, []). 
decimal_snafu(X, [D|Ds]) :-
  length(Ds, N),
  D in -2 .. 2,
  X #= X0 + D * 5^N,
  decimal_snafu(X0, Ds).

% Parsing

snafu_digit(-1) --> "-".
snafu_digit(-2) --> "=".
snafu_digit(D)  --> [C], { member(C, "012"), number_chars(D, [C]) }.

snafu([D|Ds]) --> snafu_digit(D), snafu(Ds).
snafu([D])    --> snafu_digit(D).

snafus([S|Ss]) --> snafu(S), "\n", snafus(Ss).
snafus([])     --> [].
