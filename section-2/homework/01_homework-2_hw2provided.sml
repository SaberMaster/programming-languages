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

fun card_color (suit, rank) =
  case suit
   of Diamonds => Red
   | Hearts => Red
   | _ => Black

fun card_value (suit, rank) =
  case rank
   of Num x => x
   | Ace => 11
   | _ => 10

fun remove_card (cs, c, e) =
  case cs
   of [] => raise e
    | x::xs' => if c = x
               then xs'
               else x::remove_card(xs', c, e)

fun all_same_color (cs) =
  case cs
   of x::y::z => if card_color(x) = card_color(y)
                then all_same_color(y::z)
                else false
   | _ => true

fun sum_cards (cs) =
  let
      fun sum_card_and_acc (cs, acc) =
        case cs
         of [] => acc
          | x::xs' => sum_card_and_acc(xs', acc + card_value(x))
  in
      sum_card_and_acc(cs, 0)
  end

fun score (cs, goal) =
  let
      val delta = sum_cards(cs) - goal
  in
      (if delta > 0
      then 3 * delta
      else ~delta) div (if all_same_color(cs)
                                 then 2 
                                 else 1)
  end

fun officiate (cs, ml, goal) =
  let
      fun game_run_next_round(cs, hl, ml) =
        case ml
         of [] => score(hl, goal)
         | hd1::tl1 => case hd1
                       of Discard card => game_run_next_round(cs,
                                                             remove_card(hl, card, IllegalMove),
                                                             tl1)
                        | Draw => case cs
                                  of [] => score(hl, goal)
                                   | x::xs' => if sum_cards(x::hl) > goal
                                               then score(x::hl, goal)
                                               else game_run_next_round(xs',
                                                                        x::hl,
                                                                        tl1)

  in 
      game_run_next_round(cs, [], ml)
  end

fun score_challenge (cs, goal) =
  let

      fun score_change (ace_num) =
        let
            val delta = sum_cards(cs) - goal - 10 * ace_num
        in
            (if delta > 0
             then 3 * delta
             else ~delta) div (if all_same_color(cs)
                              then 2
                              else 1)
        end

      fun get_min_score (cs, acc, min_score) =
        case cs
         of [] => min_score
          | hd1::tl1 => case hd1
                        of (_, Ace) => let
                            val new_score = score_change(acc + 1)
                        in
                            if new_score < min_score
                            then get_min_score(tl1, acc + 1, new_score)
                            else get_min_score(tl1, acc + 1, min_score)
                        end
                         | _ => get_min_score(tl1, acc, min_score)

  in
      get_min_score(cs, 0, score_change(0))
  end


fun officiate_challenge (cs, ml, goal) =
  let

      fun card_value_min (suit, rank) =
        case rank
         of Num x => x
          | Ace => 1
          | _ => 10

      fun sum_cards_min (cs) =
        let
            fun sum_card_and_acc (cs, acc) =
              case cs
               of [] => acc
                | x::xs' => sum_card_and_acc(xs', acc + card_value_min(x))
        in
            sum_card_and_acc(cs, 0)
        end

      fun game_run_next_round(cs, hl, ml) =
        case ml
         of [] => score_challenge(hl, goal)
          | hd1::tl1 => case hd1
                        of Discard card => game_run_next_round(cs,
                                                              remove_card(hl, card, IllegalMove),
                                                              tl1)
                         | Draw => case cs
                                    of [] => score_challenge(hl, goal)
                                                           (* here only if there is no sum that is less than or equal to the goal. the min sum is greater than goal *)
                                     | x::xs' => if sum_cards_min(x::hl) > goal
                                                 then score_challenge(x::hl, goal)
                                                 else game_run_next_round(xs',
                                                                          x::hl,
                                                                          tl1)

  in
      game_run_next_round(cs, [], ml)
  end

fun careful_player (cs, goal) =
  let
      fun have_card_with_this_value_and_discard (hl, value, mv_hd, mv_tl) =
        case hl
         of [] => mv_hd @ mv_tl
          | x::xs' => case mv_tl
                      of [] =>  mv_hd @ mv_tl
                       | hd1::tl1 => if value = card_value(x)
                                    then mv_hd @ hd1::(Discard x)::tl1 @ [Draw]
                                    else have_card_with_this_value_and_discard(xs',
                                                                               value,
                                                                               mv_hd @ [hd1],
                                                                               tl1)

      fun try_draw_one_more (cs, hl, mv) =
        let
            val tmp_score = sum_cards(hl)
        in
            if goal - tmp_score = 0
            then mv
            else if goal - tmp_score > 10
            then
                case cs
                 of [] => mv @ [Draw]
                  | hd1::tl1 => try_draw_one_more(tl1,
                                                 hl @ [hd1],
                                                 mv @ [Draw])
            else
                case cs
                 of [] => mv
                 | hd1::tl1 => have_card_with_this_value_and_discard(hl @ [hd1],
                                                                    tmp_score + card_value(hd1) - goal,
                                                                    [],
                                                                    mv)
        end
  in
      try_draw_one_more(cs, [], [])
  end
