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

fun longest_string_helper f xs =
  List.foldl (fn (x, acc) => if f (String.size acc, String.size x)
                            then acc
                            else x) "" xs

val longest_string3 = longest_string_helper (fn (x, y) => x >= y)

val longest_string4 = longest_string_helper (fn (x, y) => x > y)

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
