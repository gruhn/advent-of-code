:- use_module(library(dcgs)).
:- use_module(library(clpz)).
:- use_module(library(lists)).
:- use_module(library(pio), [ phrase_from_file/2 ]).
:- use_module(utils, [ integer//1 ]).

manhattan_dist(xy(X1,Y1), xy(X2,Y2), Dist) :-
  Dist #= abs(X1 - X2) + abs(Y1 - Y2).

% part1(N) :-
%   input(Sensors),

point_outside_sensor(xy(X,Y), sensor(Sensor, Beacon)) :-
  manhattan_dist(Sensor, Beacon, MaxReach),
  manhattan_dist(Sensor, xy(X,Y), Dist),
  Dist #> MaxReach.

part2(X, Y) :- 
  input(Sensors),
  X in 0 .. 4000000,
  Y in 0 .. 4000000,
  maplist(point_outside_sensor(xy(X,Y)), Sensors),
  p(Sensors, xy(X,Y)),
  labeling([ff], [X,Y]).

% Parsing

point(xy(X,Y)) --> "x=", integer(X), ", y=", integer(Y).

sensor(sensor(Sensor, Beacon)) --> 
  "Sensor at ", point(Sensor), ": ",
  "closest beacon is at ", point(Beacon).

sensors([S|Ss]) --> sensor(S), "\n", sensors(Ss).
sensors([S])    --> sensor(S).

input(Ss) :- phrase_from_file(sensors(Ss), "input/15.txt").