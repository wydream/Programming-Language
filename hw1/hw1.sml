(* Author: Yaodi Wu *)
val months_name = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
val months_days = [31,28,31,30,31,30,31,31,30,31,30,31]
val months_days_leap = [31,29,31,30,31,30,31,31,30,31,30,31]

(* 1 *)
fun is_older (date1 : int * int * int, date2 : int * int * int) = 
	if (#1 date1) > (#1 date2)
	then false
	else if (#1 date1) < (#1 date2)
		then true
		else if (#2 date1) > (#2 date2)
			then false
			else if (#2 date1) < (#2 date2)
				then true
				else if (#3 date1) > (#3 date2)
					then false
					else true

(* 2 *)
fun number_in_month(datelist : (int * int * int) list, month : int) = 
	if null (tl datelist)
	then if (#2 (hd datelist) = month)
		then 1
		else 0
	else if (#2 (hd datelist) = month)
		then 1 + number_in_month(tl datelist, month)
		else number_in_month(tl datelist, month)

(* 3 *)
fun number_in_months(datelist : (int * int * int) list, monthlist : int list) = 
	if null (tl monthlist)
	then number_in_month(datelist, hd monthlist)
	else number_in_month(datelist, hd monthlist) + number_in_months(datelist, tl monthlist)

(* 4 *)
fun dates_in_month(datelist : (int * int * int) list, month : int) = 
	if null (tl datelist)
	then if (#2 (hd datelist) = month)
		then datelist
		else []
	else if (#2 (hd datelist) = month)
		then (hd datelist) :: dates_in_month(tl datelist, month)
		else dates_in_month(tl datelist, month)

(* 5 *)
fun dates_in_months(datelist : (int * int * int) list, monthlist : int list) = 
	if null (tl monthlist)
	then dates_in_month(datelist, hd monthlist)
	else dates_in_month(datelist, hd monthlist) @ dates_in_months(datelist, tl monthlist)

(* 6 *)
fun get_nth(strlist : string list, n : int) = 
	if (n = 1)
	then (hd strlist)
	else get_nth(tl strlist, n-1)

(* 7 *)
fun get_month_name(months_name : string list , n : int) =
	if (n = 1)
	then hd months_name
	else get_month_name(tl months_name, n-1)

fun date_to_string(date : int * int * int) =
	get_month_name(months_name, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)

(* 8 *)
fun number_before_reaching_sum(sum : int, elements : int list) =
	if null elements
	then 0
	else if (sum > hd elements)
		then 1 + number_before_reaching_sum(sum - hd elements, tl elements)
		else 0

(* 9 *)
fun what_month(days : int) =
	number_before_reaching_sum(days, months_days) + 1

(* 10 *)
fun month_range(day1 : int ,day2 : int) =
	if (day1 > day2)
	then []
	else let
		fun gen_month(pos1 : int, pos2 : int) =
			if (pos1 = pos2)
			then [what_month pos1]
			else what_month pos1 :: gen_month(pos1 + 1, pos2)
	in
		gen_month(day1, day2)
	end

(* 11 *)
fun oldest(datelist : (int * int * int) list) =
	if null datelist
	then NONE
	else if null (tl datelist)
		then SOME (hd datelist)
		else let
			val t = oldest(tl datelist)
			val isOlder = is_older(hd datelist, valOf t)
		in
			if isOlder
			then SOME (hd datelist)
			else t
		end

(* 12 *)
fun is_in_list(target : int, data : int list) =
	if null data
	then false
	else if (hd data = target)
		then true
		else is_in_list(target, tl data)

fun remove_duplicates(data : int list) =
	if null (tl data)
	then [hd data]
	else let
		val result = remove_duplicates(tl data)
	in
		if is_in_list(hd data, result)
		then result
		else (hd data) :: result
	end

fun number_in_months_challenge(datelist : (int * int * int) list, monthlist : int list) = 
	number_in_months(datelist, remove_duplicates(monthlist))

fun dates_in_months_challenge(datelist : (int * int * int) list, monthlist : int list) = 
	dates_in_months(datelist, remove_duplicates(monthlist))

(* 13 *)
fun is_leap_year(year : int) =
	if (year mod 4 = 0 andalso year mod 100 <> 0) orelse (year mod 400 = 0)
	then true
	else false

fun days_of_month(month : int, months_dayslist : int list) =
	if (month = 1)
	then hd months_dayslist
	else days_of_month(month-1, tl months_dayslist)

fun reasonable_date(date : int * int * int) =
	if (#1 date) <= 0 orelse (#2 date) <=0 orelse (#2 date) > 12 orelse (#3 date) <= 0 orelse (#3 date) > 31
	then false
	else if is_leap_year(#1 date)
		then 
			if (#3 date) > days_of_month((#2 date), months_days_leap)
			then false
			else true
		else
			if (#3 date) > days_of_month((#2 date), months_days)
			then false
			else true