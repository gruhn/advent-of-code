
part1(Result) :-
    parse_input(Matrix),
    gamma_rate(Matrix, GR),
    epsilon_rate(Matrix, ER),
	decimal(GR, GRDec),
	decimal(ER, ERDec),
	Result is GRDec * ERDec.

part2(Result) :-
    parse_input(Matrix),
    oxygen_generator_rate(Matrix, OGR),
    co2_scrubber_rate(Matrix, CSR),
	decimal(OGR, OGRDec),
	decimal(CSR, CSRDec),
	Result is OGRDec * CSRDec.

decimal([], 0).
decimal([B|Bs], Decimal) :-
    length(Bs, Exp),
    decimal(Bs, Rest),
    Decimal is B * 2 ^ Exp + Rest.

count_excess([], 0).
count_excess([0|Ys], Count) :-
    count_excess(Ys, C),
    Count is C - 1.
count_excess([1|Ys], Count) :-
    count_excess(Ys, C),
    Count is C + 1.

most_common(Ys, MC) :-
    count_excess(Ys, C),
    MC is (sign(C) + 1) / 2.

first_column(Matrix, Column) :-
    maplist(nth0(0), Matrix, Column).

rest_columns([], []).
rest_columns([[_|[]]|_], []).
rest_columns([[_|Row]|Rs], [Row|Rss]) :-
    rest_columns(Rs, Rss).

gamma_rate([], []).
gamma_rate(Matrix, [MostCommon|Rate]) :-
    first_column(Matrix, FirstColumn),
    most_common(FirstColumn, MostCommon),
    rest_columns(Matrix, RestColumns),
    gamma_rate(RestColumns, Rate).

epsilon_rate([], []).
epsilon_rate(Matrix, [LeastCommon|Rate]) :-
	first_column(Matrix, FirstColumn),
    most_common(FirstColumn, MostCommon),
    LeastCommon is 1 - MostCommon,
    rest_columns(Matrix, RestColumns),
    epsilon_rate(RestColumns, Rate).

is_head(X, [X|_]).

oxygen_generator_rate([], []).
oxygen_generator_rate([Rate], Rate).
oxygen_generator_rate(Matrix, [R|Rate]) :-
    first_column(Matrix, FirstColumn),
    most_common(FirstColumn, MostCommon),
    R is ceiling(MostCommon),
    include(is_head(R), Matrix, FilteredRows),
    rest_columns(FilteredRows, SubMatrix),
    oxygen_generator_rate(SubMatrix, Rate).

co2_scrubber_rate([], []).
co2_scrubber_rate([Rate], Rate).
co2_scrubber_rate(Matrix, [R|Rate]) :-
    first_column(Matrix, FirstColumn),
    most_common(FirstColumn, MostCommon),
    R is floor(1 - MostCommon),
    include(is_head(R), Matrix, FilteredRows),
    rest_columns(FilteredRows, SubMatrix),
    co2_scrubber_rate(SubMatrix, Rate).
    

% parse input

parse_input(Output) :-
	input(Codes), 
    phrase(lines(Output), Codes).

lines([L|Ls]) --> digits(L), "\n", !, lines(Ls).
lines([]) --> [].

digits([D|T]) --> digit(D), !, digits(T). 
digits([]) --> [].

digit(0) --> "0".
digit(1) --> "1".

input(`00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
`).