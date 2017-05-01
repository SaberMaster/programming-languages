fun is_older (x : int * int * int,
              y : int * int * int) =
  if (#1 x) = (#1 y)
  then
      if (#2 x) = (#2 y)
      then (#3 x) < (#3 y)
      else (#2 x) < (#2 y)
  else (#1 x) < (#1 y)

fun number_in_month (dates : (int * int * int) list,
                     month : int) =
  if null dates
  then 0
  else
      (if (#2 (hd dates)) = month then 1 else 0) +
      number_in_month((tl dates), month)

fun number_in_months (dates : (int * int * int) list,
                      months : int list) =
  if null months
  then 0
  else
      number_in_month(dates, (hd months)) +
      number_in_months(dates, (tl months))


fun dates_in_month (dates : (int * int * int) list,
                    month : int) =
  if null dates
  then []
  else
      if (#2 (hd dates)) = month
      then (hd dates) :: dates_in_month((tl dates), month)
      else dates_in_month((tl dates), month)


fun merge_date_list (x : (int * int * int) list,
                     y : (int * int * int) list) =
  if null x
  then y
  else (hd x) :: merge_date_list((tl x), y)

fun dates_in_months (dates : (int * int * int) list,
                     months : int list) =
  if null months
  then []
  else
      merge_date_list(dates_in_month(dates, (hd months)),
                      dates_in_months(dates, (tl months)))

fun get_nth (string_list : string list,
             n : int) =
  if n = 1
  then
      hd string_list
  else
      get_nth ((tl string_list), n - 1);


fun date_to_string (date : int * int * int) =
  let 
      val month_list = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November","December"]
  in 
      get_nth(month_list, (#2 date)) ^
      " " ^
      Int.toString((#3 date)) ^
      ", " ^
      Int.toString((#1 date))
  end

fun number_before_reaching_sum (sum : int,
                               numbers : int list) =
  let
      val is_pos = sum - (hd numbers)
  in
      if is_pos <= 0
      then
          0
      else
          1 + number_before_reaching_sum(is_pos,
                                         (tl numbers))
  end

fun what_month (day : int) =
  let
      val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
      number_before_reaching_sum(day, month_days) + 1
  end

fun month_range (day1 : int,
                 day2 : int) =
  if day1 > day2
  then []
  else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest (dates : (int * int * int ) list) =
  if null dates
  then NONE
  else
      let
          fun get_older(last_old_day : int * int * int,
                        dates : (int * int * int) list) =
            let
                val older_day =
                    if is_older(last_old_day, (hd dates))
                    then last_old_day
                    else (hd dates)
            in
                if null (tl dates)
                then older_day
                else get_older(older_day,
                               (tl dates))
            end
      in
          SOME (get_older((hd dates),
                         (tl dates)))
      end

fun remove_dup_month (months : int list) =
  let
      fun is_exist_month (month : int,
                          months : int list) =
        if null months
        then false
        else
            let
                val is_exist = (month = (hd months))
            in
                if is_exist
                then true
                else
                    if null (tl months)
                    then false
                    else is_exist_month(month, (tl months))
            end

      fun add_to_months_list (month : int) =
        if month > 12
        then []
        else
            let
                val other_month = add_to_months_list(month + 1)
            in
                if is_exist_month(month, months)
                then month :: other_month
                else other_month
            end
  in
      add_to_months_list(1)
  end

fun remove_dup_number (numbers : int list) =
  let
      fun remove_this_number(number : int,
                             numbers : int list) =
        if null numbers
        then []
        else
            let
                val other_numbers = remove_this_number(number, (tl numbers))
            in
                if number = (hd numbers)
                then other_numbers
                else (hd numbers) :: other_numbers
            end

      fun walk_numbers (numbers : int list,
                        save_number_list : int list) =
        if null numbers
        then save_number_list
        else
            walk_numbers(remove_this_number(hd numbers,
                                            tl numbers),
                         (hd numbers :: save_number_list))

  in
      walk_numbers(numbers, [])
  end

fun number_in_months_challenge (dates : (int * int * int) list,
                                months : int list) =
  number_in_months(dates,
                   remove_dup_month(months))


fun dates_in_months_challenge (dates : (int * int * int) list,
                               months : int list) =
  dates_in_months(dates,
                  remove_dup_number(months))

fun reasonable_date (date : int * int * int) =
  let
      fun check_year () =
        (#1 date) > 0

      fun check_month () =
        (#2 date) >=1 andalso
        (#2 date) <= 12

      fun check_day() =
        let
            val month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
            fun get_month_day (month : int,
                               months : int list) =
              if month = 1
              then hd months
              else get_month_day(month - 1, (tl months))

            fun check_leap_year () =
              ((#1 date) mod 400) = 0 orelse
              (not (((#1 date) mod 100) = 0) andalso
               ((#1 date) mod 4) = 0)
        in
            if (#3 date) > 0
            then
                if (#2 date) = 2 andalso
                   check_leap_year()
                then (#3 date) <= 29
                else (#3 date) <= get_month_day((#2 date), month_days)
            else false
        end
  in
      check_year() andalso
      check_month() andalso
      check_day()
  end
