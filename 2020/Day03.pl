:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).

lines([]) 	  --> eos, !.
lines([L|Ls]) --> 
    string_without("\n", Cs), eol, lines(Ls), 
    { string_chars(Cs, L) }.

grid_dims([Row|Rows], (XN,YN)) :-
    length([Row|Rows], YN),
    length(Row, XN).    

grid_coords(Grid, Coords) :-
    grid_coords_(Grid, Coords, 0, 0).

grid_coords_([], [], _, _).
grid_coords_([[]|Cols], Coords, _, Y0) :-
    Y #= Y0 + 1,
    grid_coords_(Cols, Coords, 0, Y).
grid_coords_([[.|Row]|Cols], Coords, X0, Y) :-
    X #= X0 + 1,
    grid_coords_([Row|Cols], Coords, X, Y).
grid_coords_([[#|Row]|Cols], [(X0,Y)|Coords], X0, Y) :-
    X #= X0 + 1,
    grid_coords_([Row|Cols], Coords, X, Y).

dims_line_coord((XN,YN), (DX,DY), (X,Y)) :-
    M #>= 0,
    DY*M #=< YN,
    X #= (DX*M) mod XN,
    Y #= (DY*M) mod YN.

line_coords_intersect(Slope, Coords) :-
    phrase_from_file(lines(Grid), "input/03.txt"),
    grid_dims(Grid, Dims),
    grid_coords(Grid, Coords0),
    include(dims_line_coord(Dims, Slope), Coords0, Coords).

part1(N) :-
    line_coords_intersect((3,1), Coords), length(Coords, N).

part2(N) :-
    line_coords_intersect((1,1), Cs1), length(Cs1, N1),
    line_coords_intersect((3,1), Cs2), length(Cs2, N2),
    line_coords_intersect((5,1), Cs3), length(Cs3, N3),
    line_coords_intersect((7,1), Cs4), length(Cs4, N4),
    line_coords_intersect((1,2), Cs5), length(Cs5, N5),
    N #= N1 * N2 * N3 * N4 * N5.