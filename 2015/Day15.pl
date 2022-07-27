:- use_module(library(clpz)).
:- use_module(library(lists)).

ingredients(
    [ [3, 0, 0, -3, 2] % sugar
    , [-3, 3, 0, 0, 9] % sprinkles
    , [-1, 0, 4, 0, 1] % candy
    , [0, 0, -2, 2, 8] % chocolate
    ]
).   

score([], _, 0).
score([Prop|Props], Vars, TotalScore) :-
    TotalScore #= TotalScore0 + Score,
    scalar_product(Prop, Vars, #=, Score),
    score(Props, Vars, TotalScore0).

last(X, [X]).
last(X, [_|Xs]) :- last(X, Xs).

part1(Vars, TotalScore) :-
    ingredients(Ings),

    same_length(Ings, Vars),
    Vars ins 0..100,
    sum(Vars, #=, 100),

    last(Calories, Vars),
    Calories #= 0,

    transpose(Ings, Props),
    score(Props, Vars, TotalScore).