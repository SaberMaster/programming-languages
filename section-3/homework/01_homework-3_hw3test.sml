(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C","cd"] = "bc"

val test3 = longest_string2 ["A","bc","C", "cd"] = "cd"

val test4a = longest_string3 ["A","bc","C", "cd"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
(* val test7b = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3] *)

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test8b = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7]

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b_2 = count_wild_and_variable_lengths (TupleP [Variable("a"), Variable("ab"), Wildcard]) = 4

val test9c = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true
val test10_b = check_pat (TupleP [Variable("a"), Variable("ab"), Variable("ab"), Wildcard]) = false

val test11 = match (Const(1), UnitP) = NONE
val test11_b = match (Tuple [Unit, Unit, Const(2), Unit], TupleP [Variable("a"), Variable("ab"), Variable("ab"), Wildcard]) = SOME [("a", Unit), ("ab", Unit), ("ab", Const(2))]

val test12 = first_match Unit [UnitP] = SOME []

val test_typecheck_patterns_1 = typecheck_patterns([], [TupleP [Variable "x", Variable "y"],
                                TupleP [Wildcard, Wildcard]])
                              = SOME(TupleT [Anything, Anything])

val test_typecheck_patterns_2 = typecheck_patterns([], []) = NONE

val test_typecheck_patterns_3 = typecheck_patterns([], [TupleP [Wildcard, TupleP [Wildcard, Wildcard]],
                                TupleP [Wildcard, Wildcard]])
  = SOME(TupleT [Anything, TupleT [Anything, Anything]])

val test_typecheck_patterns_4 = typecheck_patterns([], [ConstP 5, Wildcard, ConstP 3, Variable "x"])
  = SOME(IntT)

val test_typecheck_patterns_5 = typecheck_patterns([], [ConstP 5, UnitP]) = NONE

val test_typecheck_patterns_6 = typecheck_patterns([("c", "t", IntT)], [ConstructorP("c", ConstP 5), ConstP 5])
  = NONE

val test_typecheck_patterns_7 = typecheck_patterns([], [TupleP [Wildcard], TupleP [Wildcard, Wildcard]])
  = NONE

val test_typecheck_patterns_8 = typecheck_patterns([], [TupleP [ConstP 3], TupleP [UnitP]])
  = NONE

val test_typecheck_patterns_9 = typecheck_patterns([], [TupleP [Wildcard, ConstP 1], TupleP [Wildcard, TupleP [Wildcard]]])
  = NONE

val test_typecheck_patterns_10 = typecheck_patterns([], [ConstructorP("c", Variable "x")])
  = NONE

val test_typecheck_patterns_11 = typecheck_patterns([("c", "t", TupleT[IntT, Anything])],
                           [ConstructorP("c", TupleP [ConstP 4, Variable "x"])])
  = SOME(Datatype "t")

val test_typecheck_patterns_12 = typecheck_patterns([("c1", "t1", UnitT), ("c2", "t2", UnitT)],
                           [ConstructorP("c1", UnitP), ConstructorP("c2", UnitP)])
  = NONE

val test_typecheck_patterns_13 = typecheck_patterns([("c1", "t1", UnitT), ("c2", "t2", UnitT)],
                           [ConstructorP("c1", UnitP), ConstructorP("c1", UnitP)])
  = SOME(Datatype "t1")

val test_typecheck_patterns_14 = typecheck_patterns([("c", "t", TupleT[Anything, Anything])],
                           [ConstructorP("c", TupleP [ConstP 4, Variable "x"])])
  = NONE

val test_typecheck_patterns_15 = typecheck_patterns([("c", "t", IntT)], [
                           TupleP [Wildcard, TupleP [ConstP 3, Wildcard]],
                           TupleP [Wildcard, TupleP [Variable "x", UnitP]],
                           TupleP [ConstructorP("c", ConstP 13), Wildcard]
                           ])
  = SOME(TupleT [Datatype "t", TupleT [IntT, UnitT]])

val test_typecheck_patterns_16 = typecheck_patterns([("c1", "t", TupleT[IntT, Datatype "t"]), ("c2", "t", UnitT)],
  [ConstructorP("c1", TupleP [ConstP 5, ConstructorP("c2", UnitP)]), ConstructorP("c2", UnitP)])
  = SOME(Datatype "t")

val test_typecheck_patterns_17 = typecheck_patterns([("c", "t", TupleT[IntT, Datatype "t"]), ("c", "t", UnitT)],
  [ConstructorP("c", TupleP [ConstP 5, ConstructorP("c", UnitP)]), ConstructorP("c", UnitP)])
  = SOME(Datatype "t")

val test_typecheck_patterns_18 = typecheck_patterns([("foo1","bar1",Datatype "bar2"),("foo2","bar2",UnitT)],
                                  [ConstructorP("foo1", Variable "x")]) = SOME (Datatype "bar1")
