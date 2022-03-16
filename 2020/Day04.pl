% :- set_prolog_flag(double_quotes, chars).
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(utils).

byr_value(Val) --> uinteger(Val), whites, 
  { Val in 1920 .. 2002 }.

iyr_value(Val) --> uinteger(Val), whites, 
  { Val in 2010 .. 2020 }.

eyr_value(Val) --> uinteger(Val), whites, 
  { Val in 2020 .. 2030 }.

hgt_value(Val) --> uinteger(Val), lexeme("cm"), !, { Val in 150 .. 193 }.
hgt_value(Val) --> uinteger(Val), lexeme("in"), !, { Val in 59 .. 76 }.

hcl_value(Val) --> "#", hex_number(Chars), whites,
  { length(Chars, 6), atom_chars(Val, Chars) }.

ecl_value(Val) --> lexeme(Chars), 
  { atom_codes(Val, Chars), member(Val, [amb, blu, brn, gry, grn, hzl, oth])}.

pid_value(Val) --> lexeme(Chars),
  { length(Chars, 9), number_codes(Val, Chars) }.

cid_value(Val) --> lexeme(Chars), 
  { atom_chars(Val, Chars) }.

pair(byr-Val) --> "byr:", byr_value(Val), !.
pair(iyr-Val) --> "iyr:", iyr_value(Val), !.
pair(eyr-Val) --> "eyr:", eyr_value(Val), !.
pair(hgt-Val) --> "hgt:", hgt_value(Val), !.
pair(hcl-Val) --> "hcl:", hcl_value(Val), !.
pair(ecl-Val) --> "ecl:", ecl_value(Val), !.
pair(pid-Val) --> "pid:", pid_value(Val), !.
pair(cid-Val) --> "cid:", cid_value(Val), !.
pair(byr-invalid) --> "byr:", lexeme(_).
pair(iyr-invalid) --> "iyr:", lexeme(_).
pair(eyr-invalid) --> "eyr:", lexeme(_).
pair(hgt-invalid) --> "hgt:", lexeme(_).
pair(hcl-invalid) --> "hcl:", lexeme(_).
pair(ecl-invalid) --> "ecl:", lexeme(_).
pair(pid-invalid) --> "pid:", lexeme(_).
pair(cid-invalid) --> "cid:", lexeme(_).

pairs([])     --> [].
pairs([P|Ps]) --> pair(P), pairs(Ps).
pairs([P|Ps]) --> pair(P), eol, pairs(Ps).

passports([])       --> eos, !.
passports([Ps|PPs]) --> pairs(Ps), eol, eol, passports(PPs).

fields([ byr, iyr, eyr, hgt, hcl, ecl, pid, cid ]).

passport_with_required_fields(Pairs) :-
  fields(AllFields),
  pairs_keys(Pairs, GivenFields),
  select(cid, AllFields, RequiredFields),
  subtract(RequiredFields, GivenFields, []).

passport_valid_fields(Pairs) :-
  pairs_values(Pairs, Values),
  include(=(invalid), Values, []).

solution(Part1, Part2) :-
  phrase_from_file(passports(PPs0), "input/04.txt"),

  include(passport_with_required_fields, PPs0, PPs1),
  length(PPs1, Part1),

  include(passport_valid_fields, PPs1, PPs2),
  length(PPs2, Part2).
