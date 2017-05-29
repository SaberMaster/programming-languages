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



fun typecheck_patterns (ts, ps) =
  let

      fun type_change p xs =
        first_answer (fn (b, c : typ) => if p = c
                                        then SOME(Datatype b)
                                        else NONE) xs
        handle NoAnswer => p

      fun type_match (p, xs) =
        case p of
            UnitP => SOME (type_change UnitT xs)
          | ConstP _ => SOME (type_change IntT xs)
          | TupleP ps => (case all_answers (fn x => case type_match(x, xs)
                                                     of SOME v => SOME [v]
                                                     | NONE => NONE) ps
                           of SOME v => SOME (type_change (TupleT v) xs)
                           | NONE => NONE)
          (* | ConstructorP(s, p) => type_match(p, ((first_answer (fn (a, b, c) => if s = a *)
          (*                                                                        then SOME (b, c) *)
          (*                                                                     else NONE) *)
          (*                                                     ts)::xs *)
          (*                                       handle NoAnswer => xs)) *)

          | ConstructorP(s, p) => let val newX = SOME(first_answer (fn (a, b, c) => if s = a
                                                                             then SOME (b, c)
                                                                             else NONE) ts)
                                                handle NoAnswer => NONE
                                 in
                                     case newX
                                      of NONE =>  NONE
                                       | SOME (b, c) => case type_match(p, (b, c)::xs)
                                                        of NONE => NONE
                                                         | SOME t => if (Datatype b) = t
                                                                    then SOME t
                                                                    else NONE
                                 end
          | _ => SOME (Anything)

      fun fold_helper2 f acc xs =
        case xs
         of [] => SOME acc
          | x::xs' => case f (acc, x)
                      of NONE => NONE
                       | SOME v => fold_helper2 f v xs'

      (* fun pattern_checker pair = *)
      (*   case pair of *)
      (*       (UnitP, UnitP) => SOME UnitP *)
      (*     (* here ConstP i match ConstP j? *) *)
      (*     (* | (ConstP i, ConstP j) => if i = j *) *)
      (*     (*                          then SOME (ConstP i) *) *)
      (*     (*                          else NONE *) *)
      (*     | (ConstP i, ConstP j) => SOME (ConstP i) *)
      (*     | (TupleP ps1, TupleP ps2) => if List.length ps1 = List.length ps2 *)
      (*                                   then (case (all_answers (fn x => case pattern_checker(x) *)
      (*                                                                    of SOME v => SOME [v] *)
      (*                                                                    | NONE => NONE) *)
      (*                                                           (ListPair.zip(ps1, ps2))) *)
      (*                                        of SOME v => SOME (TupleP v) *)
      (*                                         | NONE => NONE) *)
      (*                                  else NONE *)
      (*     | (ConstructorP(s1, p1), ConstructorP(s2, p2)) => if s1 = s2 *)
      (*                                                       then pattern_checker(p1, p2) *)
      (*                                                       else NONE *)
      (*     | (v, Wildcard) => SOME v *)
      (*     | (v, Variable _) => SOME v *)
      (*     | (Wildcard, v) => SOME v *)
      (*     | (Variable _, v) => SOME v *)
      (*     | _ => NONE *)


      fun pattern_checker pair =
        case pair of
            (UnitT, UnitT) => SOME UnitT
          | (IntT, IntT) => SOME IntT
          | (TupleT ps1, TupleT ps2) => if List.length ps1 = List.length ps2
                                        then (case (all_answers (fn x => case pattern_checker(x)
                                                                         of SOME v => SOME [v]
                                                                         | NONE => NONE)
                                                                (ListPair.zip(ps1, ps2)))
                                             of SOME v => SOME (TupleT v)
                                              | NONE => NONE)
                                       else NONE
          | (Datatype(s1), Datatype(s2)) => if s1 = s2
                                           then SOME (Datatype(s1))
                                           else NONE
          | (v, Anything) => SOME v
          | (Anything, v) => SOME v
          | _ => NONE

  in

      (* case ps *)
      (*  of [] => NONE *)
      (*  | p::ps' => case (fold_helper2 pattern_checker p ps') *)
      (*              of NONE => NONE *)
      (*               | SOME v => type_match (v, []) *)

      case (all_answers (fn p => case type_match(p, [])
                                 of SOME v => SOME [v]
                                 | NONE  => NONE) ps)
       of NONE => NONE
        | SOME i => case i
                    of [] => NONE
                    | a::ax' => fold_helper2 pattern_checker a ax'

  end
