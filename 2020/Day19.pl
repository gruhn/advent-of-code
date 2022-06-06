match(Message) :-
    atom_chars(Message, CharList),
    phrase(rule(0), CharList).

match_count([], 0).
match_count([Message|OtherMessages], Count) :-
	match(Message), !,
    match_count(OtherMessages, CountYet),
    Count is CountYet + 1.
match_count([_|OtherMessages], Count) :-
    match_count(OtherMessages, Count).

solution(Count) :-
    messages(MessageList),
    match_count(MessageList, Count).

% INPUT:

rule(77) --> rule(30), rule(112) | rule(20), rule(13).
rule(121) --> rule(43), rule(20) | rule(123), rule(30).
rule(42) --> rule(57), rule(30) | rule(101), rule(20).
rule(30) --> [a].
rule(50) --> rule(65), rule(20) | rule(134), rule(30).
rule(37) --> rule(89), rule(20) | rule(43), rule(30).
rule(106) --> rule(16), rule(30) | rule(54), rule(20).
rule(17) --> rule(30), rule(84) | rule(20), rule(35).
rule(129) --> rule(89), rule(20) | rule(96), rule(30).
rule(123) --> rule(30), rule(30) | rule(20), rule(95).
rule(20) --> [b].
rule(115) --> rule(20), rule(70) | rule(30), rule(93).
rule(112) --> rule(30), rule(90) | rule(20), rule(123).
rule(4) --> rule(20), rule(55) | rule(30), rule(5).
rule(72) --> rule(20), rule(90) | rule(30), rule(123).
rule(51) --> rule(30), rule(20) | rule(20), rule(95).
rule(24) --> rule(131), rule(30) | rule(7), rule(20).
rule(94) --> rule(121), rule(20) | rule(15), rule(30).
rule(117) --> rule(133), rule(30) | rule(102), rule(20).
rule(92) --> rule(28), rule(20) | rule(43), rule(30).
rule(70) --> rule(87), rule(30) | rule(96), rule(20).
rule(88) --> rule(109), rule(20) | rule(36), rule(30).
rule(35) --> rule(20), rule(36) | rule(30), rule(61).
rule(96) --> rule(30), rule(95) | rule(20), rule(20).
rule(47) --> rule(96), rule(20) | rule(66), rule(30).
rule(6) --> rule(20), rule(55) | rule(30), rule(123).
rule(130) --> rule(30), rule(29) | rule(20), rule(88).
rule(68) --> rule(5), rule(20) | rule(89), rule(30).
rule(66) --> rule(95), rule(95).
rule(75) --> rule(96), rule(30) | rule(55), rule(20).
rule(3) --> rule(20), rule(90) | rule(30), rule(5).
rule(7) --> rule(20), rule(53) | rule(30), rule(123).
rule(86) --> rule(30), rule(5) | rule(20), rule(53).
rule(105) --> rule(20), rule(87).
rule(46) --> rule(134), rule(20) | rule(87), rule(30).
rule(102) --> rule(30), rule(111) | rule(20), rule(103).
rule(93) --> rule(87), rule(20) | rule(87), rule(30).
rule(79) --> rule(116), rule(20) | rule(71), rule(30).
rule(8) --> rule(42).
rule(33) --> rule(5), rule(20) | rule(87), rule(30).
rule(107) --> rule(96), rule(20) | rule(123), rule(30).
rule(90) --> rule(20), rule(30).
rule(110) --> rule(20), rule(59) | rule(30), rule(32).
rule(16) --> rule(30), rule(68) | rule(20), rule(39).
rule(52) --> rule(4), rule(20) | rule(38), rule(30).
rule(71) --> rule(96), rule(30) | rule(51), rule(20).
rule(38) --> rule(96), rule(20) | rule(90), rule(30).
rule(28) --> rule(20), rule(30) | rule(30), rule(30).
rule(27) --> rule(30), rule(64) | rule(20), rule(24).
rule(91) --> rule(30), rule(27) | rule(20), rule(113).
rule(1) --> rule(20), rule(90) | rule(30), rule(134).
rule(54) --> rule(105), rule(20) | rule(18), rule(30).
rule(0) --> rule(8), rule(11).
rule(73) --> rule(65), rule(20) | rule(55), rule(30).
rule(132) --> rule(30), rule(17) | rule(20), rule(19).
rule(41) --> rule(20), rule(51) | rule(30), rule(22).
rule(45) --> rule(22), rule(30) | rule(53), rule(20).
rule(14) --> rule(124), rule(20) | rule(73), rule(30).
rule(22) --> rule(30), rule(30).
rule(23) --> rule(87), rule(30) | rule(55), rule(20).
rule(10) --> rule(30), rule(89) | rule(20), rule(134).
rule(120) --> rule(56), rule(30) | rule(9), rule(20).
rule(11) --> rule(42), rule(31).
rule(109) --> rule(95), rule(134).
rule(127) --> rule(30), rule(53) | rule(20), rule(123).
rule(61) --> rule(20), rule(53) | rule(30), rule(90).
rule(103) --> rule(127), rule(30) | rule(23), rule(20).
rule(116) --> rule(90), rule(20) | rule(87), rule(30).
rule(134) --> rule(20), rule(20) | rule(30), rule(20).
rule(114) --> rule(20), rule(63) | rule(30), rule(41).
rule(64) --> rule(20), rule(118) | rule(30), rule(25).
rule(12) --> rule(30), rule(48) | rule(20), rule(3).
rule(111) --> rule(20), rule(23) | rule(30), rule(72).
rule(60) --> rule(79), rule(30) | rule(120), rule(20).
rule(59) --> rule(20), rule(28) | rule(30), rule(87).
rule(65) --> rule(20), rule(30) | rule(20), rule(20).
rule(44) --> rule(89), rule(30) | rule(28), rule(20).
rule(18) --> rule(65), rule(20) | rule(43), rule(30).
rule(32) --> rule(95), rule(123).
rule(83) --> rule(30), rule(66) | rule(20), rule(22).
rule(63) --> rule(20), rule(123) | rule(30), rule(55).
rule(48) --> rule(30), rule(134) | rule(20), rule(43).
rule(99) --> rule(20), rule(12) | rule(30), rule(125).
rule(43) --> rule(20), rule(20).
rule(133) --> rule(20), rule(100) | rule(30), rule(110).
rule(80) --> rule(20), rule(50) | rule(30), rule(44).
rule(125) --> rule(20), rule(98) | rule(30), rule(86).
rule(135) --> rule(128), rule(30) | rule(130), rule(20).
rule(131) --> rule(20), rule(55) | rule(30), rule(134).
rule(122) --> rule(97), rule(30) | rule(60), rule(20).
rule(25) --> rule(30), rule(43) | rule(20), rule(51).
rule(95) --> rule(20) | rule(30).
rule(55) --> rule(30), rule(95) | rule(20), rule(30).
rule(13) --> rule(43), rule(20) | rule(43), rule(30).
rule(21) --> rule(20), rule(76) | rule(30), rule(80).
rule(98) --> rule(89), rule(30) | rule(65), rule(20).
rule(81) --> rule(106), rule(30) | rule(85), rule(20).
rule(53) --> rule(30), rule(30) | rule(20), rule(20).
rule(78) --> rule(45), rule(30) | rule(37), rule(20).
rule(89) --> rule(20), rule(30) | rule(30), rule(20).
rule(9) --> rule(96), rule(30) | rule(43), rule(20).
rule(31) --> rule(2), rule(20) | rule(82), rule(30).
rule(56) --> rule(5), rule(20) | rule(96), rule(30).
rule(76) --> rule(1), rule(20) | rule(23), rule(30).
rule(62) --> rule(92), rule(30) | rule(10), rule(20).
rule(58) --> rule(30), rule(22) | rule(20), rule(43).
rule(85) --> rule(77), rule(20) | rule(67), rule(30).
rule(26) --> rule(30), rule(58) | rule(20), rule(6).
rule(101) --> rule(30), rule(81) | rule(20), rule(91).
rule(29) --> rule(83), rule(30) | rule(47), rule(20).
rule(126) --> rule(20), rule(53) | rule(30), rule(55).
rule(2) --> rule(122), rule(20) | rule(135), rule(30).
rule(15) --> rule(5), rule(30) | rule(51), rule(20).
rule(34) --> rule(20), rule(37) | rule(30), rule(116).
rule(82) --> rule(30), rule(104) | rule(20), rule(132).
rule(19) --> rule(40), rule(20) | rule(62), rule(30).
rule(108) --> rule(21), rule(20) | rule(119), rule(30).
rule(36) --> rule(30), rule(22) | rule(20), rule(87).
rule(104) --> rule(69), rule(30) | rule(99), rule(20).
rule(100) --> rule(20), rule(7) | rule(30), rule(61).
rule(39) --> rule(89), rule(20).
rule(87) --> rule(30), rule(20).
rule(113) --> rule(52), rule(30) | rule(34), rule(20).
rule(5) --> rule(30), rule(30) | rule(30), rule(20).
rule(40) --> rule(4), rule(30) | rule(107), rule(20).
rule(97) --> rule(20), rule(26) | rule(30), rule(78).
rule(67) --> rule(30), rule(131) | rule(20), rule(126).
rule(118) --> rule(20), rule(134) | rule(30), rule(66).
rule(124) --> rule(96), rule(30) | rule(22), rule(20).
rule(128) --> rule(30), rule(114) | rule(20), rule(14).
rule(119) --> rule(30), rule(49) | rule(20), rule(74).
rule(84) --> rule(129), rule(30) | rule(33), rule(20).
rule(74) --> rule(30), rule(6) | rule(20), rule(46).
rule(49) --> rule(30), rule(38) | rule(20), rule(75).
rule(57) --> rule(117), rule(30) | rule(108), rule(20).
rule(69) --> rule(20), rule(94) | rule(30), rule(115).

messages([
  aaabbaababbababbabaabbbaaabbbbaa,
  baabaabaaaabbabbbbbaaabb,
  aabbbaaabaaaabbbbaabbaaa,
  bababbbabbabbaaaaabababbbbaaaaaaaaabbaabaaaaabababaaabbaababbaab,
  baababbbbbbaaaababaaaababbabbaaaaabbbbbbbabaabaaaaabbbbb,
  abbaaabababbaaaabbaabababbbabbaa,
  aaabaaabaaabaaaaaaabbbab,
  abaaabaabbababababaaabba,
  aabbbbbbaaabbaaaabbabbbb,
  aabbbabaaaababaabbbababb,
  aaabbabbbaabbabbabaabbba,
  bbbaaaabbbabbbaaababbaabaaabaaaabbbbabba,
  babbbbaaaaabaaaabaaababbbabbaabbbaababbaabaaabab,
  aabaaaaabbaaaaaabbababbbbabaaaaabbaaaabb,
  bbbbababaaabbaaaaababbaa,
  baabaaabbbbbababababaabbaaabbabbaabbbaab,
  bbaabbabbbabbbaababbbbbabaabaabbbababababaaabaaa,
  baabbbbbaaaabbbabbbbbbbb,
  bbabbaabbbabbaaabbbaabaabaaabbbbaababaaa,
  babbbaababbbabbababaabbb,
  aabbbbbabbbbaaaabbbaabababababbb,
  aabbabaababababababbaaabaaabaabb,
  aabababbaaaaaaaababbbbaaabaaaaaa,
  abbbababaaabbaabababababbaaabbbbaaabaabb,
  baabbabaaabaaaaabbbbbaabababaaaa,
  abbbbabbbababbababbaabbbbaaaabba,
  abababbababaaabbabaabbbb,
  aaaabbbaaaaaabaabaaabbba,
  bbaabaababaaaabbaaaaaabb,
  aaabbabbaabaabbbaabbaaba,
  aaaaabaabbaabaababbabaaa,
  bbabbbbabbbaabbbabbbbaab,
  aaabbabbbaaabbbbbaabaabbbabababbaaaaaababbbbaabb,
  abaabbabbabaaabababaababaabbabbbabbbbaaaaabbbbaabbaaaaababbbabbaabbbbbabaabaabbabbbbbaabaaaabbab,
  bbbaababaaaabababaaaaaab,
  bbbbabbbaaababbbaabbbbbb,
  babbbaabaababbbaabbbbbaa,
  abababbabbbabbabbbbabaaa,
  abaabaaaaabaabbabaaabbbbbbababab,
  abababbaaabababbbababaaa,
  baaabbabbbbbbabababbaaababbbbabbbaababbbaabbbbabbbaaabbabbbbbbab,
  aababbbaabababaaaaaaabab,
  baaabbababbaaaababbabbbabaaabaaabaabbbbaaabbabaabbbaabbbabbabbabaabbaaab,
  baaaabaabbababbbbbbbbababaaabaab,
  bbabbaababbabaabaaabaaabaaaabbaaabbbbaab,
  bbbaaaaaabbaaaaabaaaaaaa,
  bbabbbaababbbaaaababababbbaabaabbbbbababbaaabbbaabbaaaabbaaaaaaa,
  abbbbabbaabbabbbaaaaabba,
  babbababbaaabbabbabbaabaaaaaaaba,
  bbbbababbababbbbabbababbaaababab,
  abbaaaababbabbbbbabbaaabaababaab,
  aabbbbbbabaabababbbabaaa,
  aaaaaaaababaaabaabaabbaaabbabbbaabaaaaaabbaabbba,
  abbbababaaaaabaaaababbbaaaabbaaa,
  bbbbbabbaabaaaaaaabaabbaaaaabbaa,
  ababbabababbbaabababbbaaabaabbbb,
  ababaabaabbabbbabbbabbbabbaaaababaabbaaa,
  ababaabaaaababbbbaaaabaabababbaaabbababb,
  aaabbbaabaabbbbbbaaababbaaaabbbabbbbbbbb,
  aaababbbaaaabaabbababaab,
  aabbbabaababbbabbaaaabab,
  abaabbbbbbabaabaaababababbbabaabbaabbaabaabababbabbaaaababaaaaaa,
  abbaaaababaaaabaabbbbbabbbaaabaaaabbabaa,
  babaaaaabbbaabababaaabbb,
  abbbbbbaabaabaaabbaaaaaabbabbbaabbabbbbbaabbbbababbababb,
  baabbababbbabbababbbbbaa,
  bbabaababbbabbabaabbbaab,
  ababbbabbbbbabaabaaaaabb,
  babbbbbbbaaaabbbabbbbaba,
  babbabaabaaabbababaababaababaaab,
  aaaababaaaababaaabbbabbaabbaaaabaaaabbab,
  abbaabbbbbabbabbababbbbbbbbabbaabaaaababbbbaabbabaaaabbbbabaaaabaaaabbab,
  baabbbbabaabbbbabaaabaaa,
  aaabbaaaaaabbabaabaaaaab,
  abbabaabbaaababbaabbabab,
  aabaabbbbaababbabbbbbaaabbbaaabb,
  aaaaaaaaaabbabbbaabaaabbabaaabababbbaababbaaabababbbbaaaaaabbbababaaaaaa,
  babbabababaaaababaaaabba,
  abbaaaaaaaabbaaabbbaabaaabaaaabbabaabaaabbbaabbbbbbaabbbbbaabababababaab,
  bbabaababaabbabbbbaaaaba,
  bbabbbaaaabaaabbbbbabbbaaabbbbba,
  babbbaaaaabbaaaababbbabb,
  bababbabbbaabaabbbbaaaaabaabbbab,
  bbbaaaaabababbaabaaaaaab,
  babababbbaabaaaaaaaabbbaabbbabbb,
  aaaaabaabbabbababbaabbba,
  aabababbabbabbaabbbaabbb,
  baaababbaabbaaaaaabbbabb,
  bbaababbaaabbbaababababbaabbaaabaaababab,
  baaaabaaababababaabbabbbabababbaaababbbbbbbaabba,
  babbbbabbbabbababaabaaabbbbaaaabababaaab,
  baaaaabaaabbbbaaabbaabbaabaabbbabababbabbaaabaaaaaabaaababaabbabbabaaaaaaaabbbbb,
  babbbaaaabbbbbbbaaaababbaabbbbbbbbbaaaabbbabbbbabaababaa,
  baabbbbbabababbaabababaabaabaabaaababbbbbbaabbaa,
  babaaababaaaaabaaabaabbabaaabbabbaaaabaabbbaabbbbbabaaabaaaaabbabbbabaab,
  abbabaaababaabaaabaaabaaababbbaababbababbaaabaaababbaaaa,
  ababaabaabbababaaaabbbbb,
  baababbabaabbbbbbbbaabaaababaaabbbbbbbaa,
  aabbaaabbbbababbababbaaa,
  babaabbabbbbbabbabbbbaba,
  abbbabbbbaaababbabaabbaabbbabaaa,
  babbbaaabaaaabaaaaabaaabbaaabbbbbabbaabaabaaabba,
  bbbbabbbbabbaababbabbbbb,
  bbbbaaaabaabbbabbbbabbaaaabbbaab,
  babbbbbababababaaaaabaaa,
  babbaabbbabaabbababaaaab,
  abbbabbabbababbbaabbbbaa,
  aaabbaababbbabbbbbaabaaa,
  babbaabaabaabaaaaaaaaaaabababbba,
  aabaaaababaaaabaababbbababbbababaabababa,
  bbbabbbbaaababbaabbbaaaabbbbbaabbabbbbbaaaabbbbaabbbaabb,
  abbabababbabbaaaabaabbba,
  abbaaaabbaaaabbabbbababaabbbabaa,
  abbaaabbbbaaababbbbaaaabbabbbbbabbbababbaababbab,
  baaababbababaababbbbabaaaabbababbaaabaaa,
  babbaaabbaabbbbbaabaaaaababbabaabaaabaabaabbbabbbabaabaa,
  aaababbbaaaababaababbaab,
  baaaabbbbaabbbaaabbaabbb,
  babbbaabaabbabababaaaaabaabbaabababaabbb,
  abbabbbbabababbababbaabaaaaaaaababbaaabbabbbbbaabbbabbabbbbababb,
  baaabbbbabbbaaaabbbaabba,
  abababaabaabaabbbbbabbbaaaabbababaabbbaabbababab,
  aaaabaababababbaaababbbabbabbaabaabbbbabbbbbbbaaabaaaaaa,
  aabaaabababbbaabbbbbbbaa,
  aabaaaabbaabaaaaaaaaabba,
  babababbaabbbaaaabbaaaba,
  abbabbbaaaababaaaabaaaaaaabbbbbbabaaabbb,
  bbbaaabaaabaababbbbaabba,
  bbabaababbbbbabbaaaababaaabaaaaaabbbbaaabbaabaaa,
  aaaababbbababbababbbbbbaaaabaaaaabbaaabaabbbbbaabababaab,
  baabbbbbaabaaaababbabbaaaaaabaaa,
  bbbbbaabbbbbabaababbbaaabababbbbbbbaaaaaabbbbaba,
  ababbaabaabbaabbaaaabbbb,
  bbbbbaabaabbbbbbbaaaaaaa,
  bbbbbaaabbbbabbbaabaaabbbabaabbb,
  bbbbbaabaaababbaaaabbaaabbbbababbabaabbabbbaaabbaaaaabbb,
  baabbbaabaabbababbababbabaaaabab,
  abaababbabbabbaaababbbba,
  aaabbaababbabababababaaabbabbbbb,
  aaababbbbabbabaaabaabaaabbbbaaba,
  baaababbbabbaaabbaabaaaabbbababb,
  abbbbbbaabbbabbaaabaaaabbbababbbaaabbbab,
  bbbbbababbbaababbababaab,
  aaaababababbbaaaabababbb,
  bbabbaababbababaaabaaababbbbababbbbbbbabbbaaabbb,
  abbbbbbbaabaabbaaabbbaab,
  bbbaababbbbbbaaaaaaabbab,
  babbbbbaaaaababbaaaaaabb,
  baabbbaabaabbababbabbabb,
  baabbababbbaababaabbabba,
  baababbabaaabbabbaaababbbabbbbbbbaababbbbabbbabbbababaab,
  bbaaabaaaaababbaaaabbbab,
  babaabbaabbbbbbbbabaabbb,
  ababaabaaaabbbaaabababaaaabaaabbbaaaaabb,
  baabbabbaabbaabbaaaabababbabaaab,
  aabaabbbaaababbbabbbbaab,
  bbbbbaaaaaabbabababbbbaaaabbbabb,
  baaabaabbaaabbbabaabaaababaabababbbababababbaaababaabaabbbbabbabaaabbabbaabbaaabaaabaaab,
  aabbbbbbaabaaabbbbabbaaabaaaaabaabbbaaaababaabaa,
  aaaabaabbababbabbbbabbbbaabbbababbbaaabaababaaaaabbabbab,
  babaababaaaabaabbaabaabbbaabbababbabbbbb,
  bbbaababbbbaabaaabbbabbbabbaaaba,
  baaaabbabbbbaaaaaaaaabbbbbabababbbbababaaababbbabbababababbbbbba,
  abbbabaaabaabbbaaabaabaabbbbbbab,
  babaaaaabaabaabaaaabaabb,
  aabababbabaabaaaaaaabbbabbaabbabaaaabbabbaaabbba,
  ababbbabbbbbbaaaaaabbaababbbaaaaaaaaabba,
  baabbabbbbabbaababbabbaaabaabbba,
  bbaaababbabbabbabbabbaabaaaaaababaabaaabaababaaa,
  baabbbbbbbbaabaaabbabaaa,
  abbabababaaabbabbaaaabab,
  ababbababaabaaabaabbbbbbbbbabaab,
  baabababbaaaabbababbaaaaabaabbaa,
  abbaaabbabbabbabaaaaabbbbabbbbaaababbbbbaababbabbaababbaabbbabba,
  abbbbababbabaabaabbaaaabbaabbabaaabbaababaaabaababaaaabbabababba,
  babbaaabbbabbabaaaabbbab,
  aabbbaaaabbbbabbaaabbbbb,
  baababbbbaaabbababbbabbbbbaaabaaaabbaaaaaaaabbab,
  babaabababababbababbbbbaabbabaaa,
  babaaabaaaabbabbaabaaababbaabaabbaababbbabaabbbabbbbabbaabbaabaa,
  baababbabababbabbbabbaabbaabbbaabbbababb,
  baabaabbababaabbaabbaaaaaaaaabba,
  aaaababbbabbaababbbbaaba,
  bbbabbbaaaaababbaabbbbaa,
  abaaaabaaaaaaaaaaaabbaaaabaabbab,
  ababbaabbabbaaaabbbbaaab,
  baaabbbbabbbaabababaabababaabbbb,
  baabaabaaabaaabbbbbaabaaaabaababaabbbbba,
  bbbbabaababbbaabaaaaaabbbaaababa,
  abbabababaabbabbababbabb,
  babababababaaaaaabbbbbaa,
  aabbbbbbbabaaaaabbbaaaaaabababbbabbbaabbababbaaa,
  babababaabaababaababaaab,
  baababbabaabbbbaaaabbbaabaaabbabababaababaaabaabababbbbbbbabababbaaabbaa,
  abbbaabaaabbbabaaaabbbbb,
  bbabbabaabbabbbaaabbabbbaababbababbabaaa,
  abbaababaaababbbabbbbbab,
  aaababbababaaabbabbababb,
  baaaaabababaaabbbaabbbaabaaabaaa,
  aabaaaabbbbbababaaababbb,
  abaaaabbbabbabbaabbbbbbbabbaabbb,
  bababaaababbbbaaaaaabababaaaababbaabbbabbaaabbabaabaaaaababaaaabaabaabbabbbaaabbaababaab,
  aabbabbbabbaaaaaaaababab,
  bbbaaaaaababbaabababbbba,
  ababababbbababbbaaabbaabbababbbabbaabbaa,
  aabbabaabaaabaabaaabaabaaaabaabbbbababbaaaababababbbbbabbbaabaaababbaabababaaaabbaaaaaba,
  abbbbabbbbaaaaaabbaabaaa,
  aaaababbbabaaabbbabaababaaabaaabaaaaabbb,
  ababbbaaabbabbaabbaaabbb,
  abaaaabbbaabbbaaaaabaaabbbabbbba,
  aaabaaaaaaabbabaabbabbaababbbbbaabaabbbb,
  aaabbabbbabbbaababaababbaaaaabbbaababbbb,
  aabaabaabaaaabbabaaabbaaabbabaaaaabbabbbbbabaaaabbbababbaabaabbb,
  abaabbaababbaaabbabaaabbbabaabbabbabbabaabbbbbabaababbaa,
  ababbbabbbbabbbbbaaaabbbabbabbbaaaaaaaab,
  abbbabbabaaabbababbbababaaabbaabaaaaabba,
  abaaaabababaaaaabbaabaabaababaaa,
  bbbbbbabbabbbbaaaaabbbabbbaaaabaaabaabab,
  bbbaaaaaabbbbbbbaabababa,
  baabaaaabbbabbbaabababbb,
  aabbbabaaaaaabaabbbabbaa,
  babaaabbaaaaabaaabbbbbbbbabaaabbaaaaabab,
  bababbbbbaabbabbbbaaabbb,
  bbaabbbaabbabbbbaabbbabb,
  abbabaabbabbaabaaababaaa,
  bababbaabaabaabbabbabbab,
  bbbaababbabbabaabaabbbbabaaaaabbbbbbaaab,
  bbbaababbabbbbaabababbba,
  babbaabbbabbabaaaabbabbbbabaaaabbbbababb,
  bbaabaababaabaaaababaaababaaabbaababbbbb,
  abbbabbbaababaaaaaabbabbabababaaaaababaaaaababbabbaababb,
  abbaaabbbaabbabbabaabbaaaababbbabbaabaabababbbabaaabbbab,
  abbbaaaaaabbbaaabbaaabbb,
  bbbbbabbabbabbaaabbabbbb,
  baabbabaaaaabababaaaaaab,
  aaaababbbbbabbbaaaababaabababbbb,
  abbbabaabbbababbbbabbbaaaaaaabaaaaaaabbbaaababab,
  baaabbbbaaabbaabbaaabbababbaababbbbaaaba,
  bbaababbbbbbbababaaababbabbabbbaaabbbabaabbbabbbbaaabaabaaabaaba,
  aaabaaaabbbbabbbbabbbabb,
  bbabbaababaababababbabbb,
  babbaaababbbbabbabbbaaabaabbabba,
  bbabbaabbabbabaababaaaaaaababbabbaaabbaababbbbbaababbbabaababbabbbabbbaabbbabbbbabaabbab,
  baabaaaaabaaaabbabbbbbbbaabbbaaabaaabbabbabbabbaaaaaaababaaababaaaaaabbb,
  bbaabbabbbaaabaabbaababa,
  aababbbabbbaabaaaaabbbab,
  baabbbbaabbabbaabbaaaabb,
  bbabbaabbaaaabaaaaaabbbb,
  baabaabaaabaabbaabababbb,
  abaabbaabbaabaabbbaabbabaaabbbaaaaaaabbb,
  bbbbbababbaaababbaabbaaa,
  bbaabbabbbabbaaabaabbbab,
  bbbaaaaaabbbbbbbaabbabaa,
  bbaaababbbabaabbaabbaaaaaaabbbbbbaaababaabbbbbbbbabaabaaaabbabab,
  aabbaabbbabbaaabaabbaaaabbababab,
  abbbabbaaaababbaabbaaaaa,
  aaabbabbbbaabaabbbabbaaaabbaaabbbabbbaba,
  abaabaaababbababaaababbbaabbbabb,
  abaaaabababbbbbbaaabaaaaabababbaaabbbbab,
  abbaabbbbbabbbbabbbaaababbabaaabbbabbbab,
  babbbbbbbabaabbabaababbaabaabaaaabbbabaababbbbab,
  bbbabbbbaabbaaaaabbaabaa,
  bbbbbaaababababbbaaababa,
  baabbbaabaabbababbbaababbbababbbabaabbbb,
  bbaababbbabaaabbabbbaaab,
  baabaaaaabbaaaaabbbaaaabbabbabbaaabbbaaaaabaabbbbbbbaaba,
  abbbababaaaaabaaababbabb,
  aaaaabaababbaaabaaaababbbaaabbbbababaaaa,
  ababbaababbabbbababbabbabbabbaababaaaaabbbbbaaaabbbaaaba,
  baaabaaabbbbabaaaababbaabbbaaabaababbabbaaabaaba,
  bababbaaababababaabbabbbaabaabaaabaaabbaabbbaaaaabbbabaa,
  abbaaabbabbaababbbaaaaaabbaaaabb,
  bbaabaabbbbaaaaabababababbabbbaaaaabbbba,
  bbaababbabbababaabbbaababbabbabbbbabaaab,
  abababaaabbbaaaabababbaaabbbaaababaaaaab,
  abbabaababbbbabbbbaabbbb,
  ababbaabbabbabababaaabbb,
  aababbbabababaababbbbbbaababbabbaababbbbbbbbaaaababbaabbbaaaaabb,
  abbbaaaaaabababbababbbba,
  babbabaaaaabbbaabaaabbaa,
  aabaaababaabbbaaaabaaababaaababbabbbaaaaaaaabbaa,
  bbaabbabbbabbabaaabbaaba,
  bababbaabbbaababbaaaabaaabbbbaab,
  bbbabbbabbbabbababbbaaab,
  abbbbbbbaabbabbbaaaabaabaabbaaabaaaabbbb,
  bbbbbabbbaababbbaabbaaba,
  ababababababababababbabb,
  baaaaaaabbabbaababaaabbb,
  bababbaaabbabababababbabaabaabbabbaababaababbbbb,
  bbabbaaabbbbaabaababababaabbbbabbabaabbbbaabbbaabbaaaababaabaabb,
  abbababaabaaaabbbabaaaab,
  bbabaabababbaabbbabbbbbbabbbbaabaabaabaa,
  bababbaaaabaaaaabbbbaabb,
  bbaabbabaabaabbbbabaabbaababaaababbaaaab,
  babaaabaaabaaaabababaabababbbaba,
  babbbbbaabababbaaaabbaabbabbabaabbaabbbb,
  abbababaabababaababaaababbbabbaa,
  aabaaaaaabbabbaabbabaababaababbaabaaabbb,
  abaabbaabbbbbaaaabbaabba,
  babaabbabaabbbbbbbabbbaababbbaabbbbababbaabbbaaabababbbbbaaababaabbbabbabbaaaabbaababaabaaaabbba,
  aaaabbbabbaaababaaabbaaaabababaababbbbababbaabbb,
  baabbabaabbaababaabbabab,
  ababaabbaabaaabaabbababb,
  aabbbababbaabbabaaaaabaa,
  ababaababbbabbbbbaaaabab,
  ababaababaaaaababbbabbbaabbbabaa,
  abaabbaaabbabababbbbabba,
  babbbbbabbaaaaaaaaaaabab,
  bbabbababbbabbbbababbbbb,
  abaababbabbaaabbbbbbababbbbbaaaa,
  abaaaabbbababbbbaababbbb,
  baabaababababbbbababbabb,
  baaaabaaaaabbabbabbabababaabbababbbbabaaabaaababbabbaaaa,
  aaaaaaaabbbabaaabbabbabaaabbbbaaabaaababababbabbabababab,
  bbbaaaabaaabaaaaaabaabbbbababaab,
  baabbbbabaaaaabaabbbaabb,
  abaaaabaabbaaaaaabaababbbaaabbba,
  ababbbaabbbaabaaaaababab,
  aaababbabbbbbabbabbbaaaabababaab,
  bbbbabbbbababbabaabbbbba,
  baaabbbbbbbbbaaabbaaabaaabaaabbaaababbab,
  bbabbaabababbbbbbbbaaabbabbbaaba,
  aaabbababaabaaabbaababbbbbaabbababbaaabbbbabbabbaabbbbab,
  abbaaaaabbabbaaaabbababb,
  baabaabbbaabbabababbaaabbaaaaababaaaabbabbbbbbaa,
  babbaabaaabababbbabbabaaabaaaaabbbaaabbb,
  babababbaabbaabbbbbbaaab,
  babaaababbbaabaaaaaabaabbbbaaaabababbaaa,
  aaabbbbbbbabababaabbabba,
  baaabbaaaaabbbbaaababbaaabbaaaaabbabbbbbabbbabbbbbbaaaaabbaaaababaaabbbbbaabbaababbbbbbababbaaaa,
  ababbababaabbabaababbbababaababbabbbaaaabbbbbbaaabbababb,
  babababababababaaaaabbab,
  aaabbaaaabbbbabbabaaabbb,
  babbbaaabababbaabaabaabaabbaababaaaabaaaababaaab,
  babaaaaaabbbbabbbbaabbba,
  bbabbaabaabbbaaaaabaaabaabbbababaabbaabaababaaaaababbbba,
  aabbbabaabbabbbababbaaaa,
  baaaabaababbabaabbbaabbb,
  aaabbabaaaabaaabaabbaaaabbaabbaa,
  abaabaaaabbabbaabbaababbabababbabbbbaabbbbaabbba,
  bbbaaaaabbabbabaabaaabaa,
  ababbbabbabbaababbaabbaa,
  aaabbaaaababbbabbaabbaaa,
  aaaaabaaabbabbaaabbbbaab,
  bbbaaaababaabaaabbbabbbabaabaaaabaabbbaaabbabaabaaaabbbb,
  babaaabababbbbbaaaaabbaa,
  baabaabbbabaabaabbaaaaab,
  baababbbbaaabbabaababbbb,
  bbaababbbbababbabbabbbbb,
  abbaababaabaabbaabbbbbbbbbaabbbbbbaababa,
  babaaaaaaaababbaabaaabab,
  babaaaaabbaabbaabaabbaabbbaabaaabbbbaabbbbbaabba,
  bbbaaabaaabbbabbabbaabbb,
  bbbbabababbaaaaaaabbbaaabbbbabaabbaaaaaabbbbaaaa,
  bbbaaaaaabaabaaabaaababa,
  aaabbababaaabbabbabaaaaaaaababababbbbaab,
  babbbbbabababbababaaabbb,
  aabababbaabbabbbbbbbbbba,
  abbabbaaabaababbbabbabbaaaaabaabaabbbbba,
  babbabbaabababbabababbbbaaababaaabbaaabbaabaabaa,
  aaaaaaaabbbabbbaabaaabba,
  baaaabaaabbbbbbaaabaabbbbbbbaaaa,
  abbbaaaabababaaabbbbbabbabaaabaaaaababbbabaababaabaaaaaa,
  abaababbabbbbbbaaabbabbbabbaaaab,
  abbabbbababbaaabbbabaaaabaababaaabbaabba,
  aaaabaabbbababbbbabbbbaabbbabbababaaaaab,
  abbaaabbabaababbbaabaababaaababa,
  abababababbaaabbbaaabbababaabaab,
  baaabbabaabaabbaaabababa,
  bbaaabaaaaaaabaababbaabb,
  ababbabaaabbabaaaabababa,
  babbbbaabbbbbaabbbaababa,
  babaaaababaabbababbabbbaabaaabababbabbaaaabbaabb,
  baaababbaaabaaabaabbabbbbaaaaaab,
  abaabbaabbabbaabaabababbabbabbbb,
  bbbaababaababbbabbababab,
  baababbabaaaabaaabababbababaaaab,
  babbaababbabaaaaababbaaa,
  aababbbaaabababbaabaabab,
  bbaabbabaabbbbbbbbababbbaaaabbbabbbbaaba,
  bbaaababbbaaabaabbbaaabaababbbbababbbabbababbbbaabaabbba,
  bbabaaaaaabbbababbaaabbb,
  baaaabaabaabbababaabaaababbbaabbbbabbaabbbbabbabaaababaaaabbaaaaaaaabbabaabbbabaabbabbaa,
  abaaaabbaabbabaaaabbbababbabaaaaaabbaaba,
  baaaaaabbbabbbbaabbbbbbbbbabaabbbabbabababaabababbbbaaabaaaabbbababbbbaa,
  aaaaaaaaaabbbabaaabbaaab,
  aababbbababaabbaaaabbbababbbaabb,
  abbbbabbaaabbbaaababaabbbabababa,
  aaaaabaabaaaabaaabbaabaa,
  bababbabbabaababbabbbabb,
  abbbbabbbaabbbabbbbaabaabbbaabbaaaaababaaaaaaaaababaaaba,
  aabaaababbbaabaabaabaaaabbbbaaababbbbaba,
  abbabaabbbaaababaaabaaba,
  bbaaababbbbbbaababbbabaa,
  bbabaaaababbbaaaabaaaaaa,
  aaabbaaabbabaababaaaabba,
  baabbabaaabbaaaaabbbbaba,
  baabaabababbaabbbbababaa,
  aaaabaabbbaaababaabbaabbabbbaaaaabaabaab,
  aaababbbaabaaaabbaaaabbbbabbbbba,
  aaabbbaababbaababbaaaaba,
  bbbabbbababbbaabbbaababa,
  abbbaaaabbabbbaaabbaabaa,
  aaaaaabbbbabbaaaabbabbbabaabbbabbbaaabaabaaaabaa,
  abababaaaaaaaaaaabbbaababaaaabbabbaabbabbabaabaaaabaabaababaabbabaabbbbb,
  ababbbaaabaabbaaabaababaabbbbbaa,
  abbabbbaababbababbbbbabaaabaabbabaabbabbbbaaaabaabaaabaa,
  ababbaabababbbabbbbbaaab,
  babbabaababaaabbbaabaabb,
  bbababbbaaabbaabbabbabaaaabaabaa,
  abaababbbaabbababaaababbbabaababbaaaaabaabbabbbb,
  ababababaaababaabbaaaabb,
  bbbaaaaaababbaababaabbba,
  babbbaababbbabbabbabaababaababbabbaababa,
  baabaabbbbaaaaaabbbaaabb,
  abbbaababbbbbabababbaabaaaaaabaaabbaaaaaabbbbaabaabbbbaaaabbbbabaababaab,
  ababaabbaabababbbbabbaabaaababaabaaaaababaaaabba,
  ababbbbbaabbaababababaababaaaaaaabbaaaaabbabbbaa,
  abbaaabbabaaaabbbbbabbbbbabbaabbbbbaaaaaabbbabbbbbbaabba,
  aaababbaaaaabaabbbaaababaaaabbbb,
  aabaaababbbbbbabaaabbabaabbaaaaabbbbbbbbababababbbaababbbabbabab,
  bbbaababbaaabbaabbbbbbaaabaaaaabaabbaababbaaaaab,
  aaaabbbaababbaabbabaabbababaaabbbaabbaab,
  bbbabababbababababbaabaa,
  bbbbabaabbaababbaaaabbbaabbbbbab,
  aaabbaaaaaaabbbaabbbbbbaabaaaabababbbaaabbaababbbaaaaaaaababbbbabababaaa,
  aaaaabbababbababbbababbbbaaababbabaabbaaaababbaabbaabbaa,
  bbabbabaaaaaabaabababbbbaaabbbba,
  ababbbaabbababbaaaaaaaab,
  babaaabbbaabbabaaaabaabb,
  babbababbbbbabaabababaab,
  bababbabbbaabaabbbbabaab,
  abbbaabaaabbbbbbaabbbbbbaabaaabbbbbbbaaaababbaaaaababaaabaaabaaa,
  abaabaaaabaababababaaabaaaabbbaabbabaaab,
  ababbbabaaaaaaaababbbbab,
  bbabbbbaabbbbabaaaaaabbbbababbaaabbbbbbbbbaabbbabbaaababababbbbbabbaabbabbabaaaabbbabbba,
  abaaaabbaaabbaabaaabaaaabbbabbbbababbbaabaabbbab,
  aabbbbbbbabbaababaabbababaabbbba,
  baaaabbbbababbabbbaabbba,
  abbbaaaaababbbababbabaaa,
  bbbabbbaaaababbaabaabbaaabaababaabbabababbaaabbaaaaaaabbabaabbabbbbaabbb
]).