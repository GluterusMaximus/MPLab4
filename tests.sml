use "hw03.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]
val test2 = only_capitals ["abacus","BAba","Cant"] = ["BAba","Cant"]
val test3 = only_capitals ["avava","dfd","ewfsa","sdf","ascc","asdf"] = []

val test4 = longest_string1 [] = ""
val test5 = longest_string1 ["abacus","BAba","Cant"] = "abacus"
val test6 = longest_string1 ["avava","dfd","ewfsadfdfdfdf","sdf","ascc","asdf"] = "ewfsadfdfdfdf"
val test7 = longest_string1 ["avava","dfd","hello","sdf","world","asdf"] = "avava"

val test8 = longest_string2 [] = ""
val test9 = longest_string2 ["abacus","BAba","Cant"] = "abacus"
val test10 = longest_string2 ["avava","dfd","ewfsadfdfdfdf","sdf","ascc","asdf"] = "ewfsadfdfdfdf"
val test11 = longest_string2 ["avava","dfd","hello","sdf","world","asdf"] = "world"

val test12 = longest_string3 [] = ""
val test13 = longest_string3 ["abacus","BAba","Cant"] = "abacus"
val test14 = longest_string3 ["avava","dfd","ewfsadfdfdfdf","sdf","ascc","asdf"] = "ewfsadfdfdfdf"
val test15 = longest_string3 ["avava","dfd","hello","sdf","world","asdf"] = "avava"

val test16 = longest_string4 [] = ""
val test17 = longest_string4 ["abacus","BAba","Cant"] = "abacus"
val test18 = longest_string4 ["avava","dfd","ewfsadfdfdfdf","sdf","ascc","asdf"] = "ewfsadfdfdfdf"
val test19 = longest_string4 ["avava","dfd","hello","sdf","world","asdf"] = "world"

val test20 = longest_capitalized [] = ""
val test21 = longest_capitalized ["abacus","BAba","Cant"] = "BAba"
val test22 = longest_capitalized ["avava","dfd","ewfsadfdfdfdf","sdf","ascc","asdf"] = ""
val test23 = longest_capitalized ["avava","Dfd","Hello","sdf","World","asdf"] = "Hello"

val test24 = rev_string "" = ""
val test25 = rev_string "Hello this will be reversed" = "desrever eb lliw siht olleH"

val test26 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test26_1 = (first_answer (fn x => if x > 3 then SOME x else NONE) [] = 4) handle NoAnswer => true

val test27 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME([2,3,4,5,6,7])
val test28 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [1,3,4,5,6,0] = NONE
val test29 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [] = SOME([])

val test30 = count_wildcards Wildcard = 1
val test31 = count_wildcards (Variable("x")) = 0
val test32 = count_wildcards (TupleP([Wildcard, Variable("dfd"), Wildcard])) = 2

val test33 = count_wild_and_variable_lengths (Variable("abc")) = 3

val test34 = count_some_var ("x", Variable("x")) = 1
val test35 = count_some_var ("y", Variable("x")) = 0

val test36 = check_pat (Variable("x")) = true
val test37 = check_pat (TupleP([Variable("df"), Variable("dfd"), Wildcard])) = true
val test38 = check_pat (TupleP([Variable("dfd"), Variable("dfd"), Wildcard])) = false

val test39 = match (Const 13, Variable("x")) = SOME [("x", Const 13)]
val test40 = match (Tuple([Const 1, Const 2, Unit]), TupleP([Variable("df"), Variable("dfd"), Wildcard])) = SOME [("df", Const 1), ("dfd", Const 2)]

val test41 = first_match Unit [UnitP] = SOME []

(***
val test2 = longest_string1 ["A","bc","C"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true

val test11 = match (Const(1), UnitP) = NONE

val test12 = first_match Unit [UnitP] = SOME []
***)
