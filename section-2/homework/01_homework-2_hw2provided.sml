(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (str, list) =
  case list
   of [] => NONE
    | hd1::tl1 => if same_string(str, hd1)
                 then SOME tl1
(* here is dicfficut for me, pay more attention*)
                 else case all_except_option(str, tl1)
                       of NONE => NONE
                        | SOME tl2 => SOME(hd1::tl2)

fun get_substitutions1 (list, str) =
  case list
   of [] => []
    | hd1::tl1 => case all_except_option(str, hd1)
                  of SOME result => result @
                                   get_substitutions1(tl1, str)
                   | _ => get_substitutions1(tl1, str)

fun get_substitutions2 (list, str) =
  let
      fun helper (list, acc) =
        case list
         of [] => acc
          | hd1::tl1 => case all_except_option(str, hd1)
                        of SOME result => helper(tl1, acc @ result)
                         | _ => helper(tl1, acc)
  in
      helper(list, [])
  end

fun similar_names (list, first_name) =
  let
      val {first=f, middle=m, last=l} = first_name
      fun get_full_name (first_list, acc_list) =
        case first_list
         of [] => acc_list
          | hd1::tl1 => get_full_name(tl1,
                                     acc_list @ [{first=hd1, middle=m, last=l}])
  in
      get_full_name(get_substitutions2(list, f), [first_name])
  end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
