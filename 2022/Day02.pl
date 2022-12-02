:- use_module(library(dcgs)).
:- use_module(library(pio)).
:- use_module(library(lists)).

part1(TotalScore) :-
    phrase_from_file(lines(Rows), "input/02.txt"),
    transpose(Rows, [Col1, Col2]),
    maplist(char_move, Col1, OpMoves),
    maplist(char_move, Col2, MyMoves),
    game_score(OpMoves, MyMoves, _, TotalScore).

part2(TotalScore) :-
    phrase_from_file(lines(Rows), "input/02.txt"),
    transpose(Rows, [Col1, Col2]),
    maplist(char_move, Col1, OpMoves),
    maplist(char_outcome, Col2, Outcomes),
    game_score(OpMoves, _, Outcomes, TotalScore).

% Parsing

char_move('A', rock).
char_move('X', rock).
char_move('B', paper).
char_move('Y', paper).
char_move('C', scissors).
char_move('Z', scissors).

char_outcome('X', lose).
char_outcome('Y', draw).
char_outcome('Z', win).

line([M1, M2]) --> [M1], " ", [M2].

lines([L|Ls]) --> line(L), "\n", lines(Ls).
lines([L])    --> line(L).

% Game Logic

beats(rock, scissors).
beats(paper, rock).
beats(scissors, paper).

round_outcome(Move, Move, draw).
round_outcome(OpMove, MyMove, lose) :- 
    beats(OpMove, MyMove).
round_outcome(OpMove, MyMove, win) :- 
    beats(MyMove, OpMove).

% Score Calculation

choice_score(rock, 1).
choice_score(paper, 2).
choice_score(scissors, 3).

outcome_score(lose, 0).
outcome_score(draw, 3).
outcome_score(win, 6). 

game_score(OpMoves, MyMoves, Outcomes, TotalScore) :-
    maplist(round_outcome, OpMoves, MyMoves, Outcomes),
    maplist(choice_score, MyMoves, ChoiceScores),
    maplist(outcome_score, Outcomes, OutcomeScores),
    sum_list(ChoiceScores, Sum1),
    sum_list(OutcomeScores, Sum2),
    TotalScore is Sum1 + Sum2.