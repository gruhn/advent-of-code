
time_now(1008832).
busses([23,x,x,x,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,449,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,13,19,x,x,x,x,x,x,x,x,x,29,x,991,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,17]).

departure(Bus, Departure) :-
    time_now(TimeNow),
	Departure is Bus - mod(TimeNow, Bus).

earliest(BusA, BusB, BusA) :-
    departure(BusA, DepartA),
    departure(BusB, DepartB),
    DepartA < DepartB, !.
earliest(_, BusB, BusB).
    
is_x(x).

solve(Next, Depart) :-
    busses(BXs),
	exclude(is_x, BXs, [B|Bs]),
	foldl(earliest, Bs, B, Next),
    departure(Next, Depart).
