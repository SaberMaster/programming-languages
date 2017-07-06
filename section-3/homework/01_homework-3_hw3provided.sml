(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
         | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end


(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals xs =
  (* is we use o we must use quote wrap all fun name  *)
  List.filter (fn x => (Char.isUpper o String.sub) (x, 0)) xs

fun longest_string1 xs =
  (* why not non-name func using can't use curry *)
  (* the std lib fold take non-name fun format is f(x, acc) *)
  List.foldl (fn (x, acc) => if String.size acc >= String.size x
                         then acc
                         else x) "" xs
fun longest_string2 xs =
  List.foldl (fn (x, acc) => if String.size acc > String.size x
                            then acc
                            else x) "" xs

fun longest_string_helper f =
  List.foldl (fn (x, acc) => if f (String.size x, String.size acc)
                            then x
                            else acc) ""

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f xs =
  case xs
   of [] => raise NoAnswer
    | x::xs' => case f x
                of NONE => first_answer f xs'
                | SOME v => v

fun fold_helper f acc xs =
  case xs
   of [] => SOME acc
    | x::xs' => case f x
                of NONE => NONE
                 | SOME v => fold_helper f (acc @ v) xs'

fun all_answers f xs = fold_helper f [] xs


(* why can use (fn _ => body) instead of (fn () => body)   *)
(* but cant use (fun abc _ = body) instead of (fun abc () = body) *)
val count_wildcards = g (fn () => 1) (fn x => 0)

val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size x)

fun count_some_var (s, p) =
  g (fn () => 0) (fn x => if x = s
                        then 1
                        else 0) p

fun check_pat p =
  let
      fun get_all_strings p =
        case p of
            Variable x        => [x]
          | TupleP ps         => List.foldl (fn (x, acc) => (get_all_strings x) @  acc) [] ps
          | ConstructorP(_,p) => get_all_strings p
          | _                 => []

      fun exist_dup xs =
        case xs
         of [] => true
          | x::xs' => if List.exists (fn z => z = x) xs'
                     then false
                     else exist_dup xs'
  in
      (exist_dup o get_all_strings) p
  end

fun match pair =
  case pair of
      (_, Wildcard) => SOME []
    | (v, Variable s) => SOME [(s, v)]
    | (Unit, UnitP) => SOME []
    | (Const x, ConstP y) => if x = y
                            then SOME []
                            else NONE
    | (Tuple vs ,TupleP ps) => if (List.length vs) = (List.length ps)
                              then all_answers match (ListPair.zip (vs, ps))
                              else NONE
    | (Constructor(s2, v), ConstructorP(s1, p)) => if s1 = s2
                                                  then match (v, p)
                                                  else NONE
    | _ => NONE

fun first_match v xs =
  SOME(first_answer (fn x => match (v, x)) xs)
  handle NoAnswer => NONE



fun typecheck_patterns (ts, ps) =
  let

      fun type_match_pre p =
        case p of
            UnitP => UnitP
          | ConstP _ => ConstP 0
          | TupleP ps => TupleP(List.map (fn x => type_match_pre x) ps)
          | ConstructorP(s, p) => ConstructorP(s, type_match_pre p)
          | _ => Wildcard




      fun pattern_compare (basic, new) =
        if basic = new
        then true
        else case (basic, new) of
                 (TupleT ps1, TupleT ps2) => if List.length ps1 = List.length ps2
                                            then List.foldl (fn (x, acc) => x andalso acc) true (List.map pattern_compare (ListPair.zip(ps1, ps2)))
                                            else raise NoAnswer
               | (v, Anything) => true
               (* | (Anything, v) => true *)
               | _ => false

      fun type_match_aft part =
        case part of
            UnitP => UnitT
          | ConstP _ => IntT
          | TupleP ps => TupleT(List.map (fn x => type_match_aft x) ps)
          | ConstructorP(s, p) => (case (List.find (fn (a, b, c) => (a = s) andalso pattern_compare(c, type_match_aft p)) ts)
                                  of NONE => raise NoAnswer
                                   | SOME (a, b, c) => Datatype b)
          | _ => Anything

      fun pattern_checker (p1, p2) =
        if p1 = p2
        then p1
        else case (p1, p2) of
                 (TupleP ps1, TupleP ps2) => if List.length ps1 = List.length ps2
                                             then TupleP(List.map pattern_checker (ListPair.zip(ps1, ps2)))
                                             else raise NoAnswer
               | (ConstructorP(s1, p1), ConstructorP(s2, p2)) => if (type_match_aft (ConstructorP(s1, p1)) = (type_match_aft (ConstructorP(s2, p2))) handle NoAnswer => false)
                                                                then ConstructorP(s1, p1)
                                                                else raise NoAnswer
               | (v, Wildcard) => v
               | (Wildcard, v) => v
               | _ => raise NoAnswer


  in
      case (List.map type_match_pre ps)
       of [] => NONE
        | x::xs' => case (SOME(List.foldl pattern_checker x xs') handle NoAnswer => NONE)
                    of NONE => NONE
                    | SOME v => SOME(type_match_aft v) handle NoAnswer => NONE
  end
