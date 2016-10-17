 (* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2



(* put your solutions for problem 1 here *)
fun all_except_option(str : string, strList : string list) =
	let fun getRestString(cutString : string, allStrings : string list, ans : string list) =
		case allStrings of [] => ans
			| astr::arest => if same_string(cutString, astr) then getRestString(cutString, arest, ans) else getRestString(cutString, arest, ans @ astr::[])
	in
		let fun cal(c_str : string, c_strList : string list) = 
		case c_strList of [] => NONE
			| s::rest => if (same_string(str, s)) then SOME(getRestString(str, strList, [])) else cal(str, rest)
		in
			cal(str, strList)
		end
	end
	

fun get_substitutions1(sll : string list list , s : string) = 
	case sll of [] => []
		| sl::rest => let val a = all_except_option(s, sl) in
			if isSome a
			then valOf a @ get_substitutions1(rest, s)
			else get_substitutions1(rest, s)
		end

fun get_substitutions2(sll : string list list , s : string) = 
	let fun aux(a : string list list , b : string list) = 
		case a of [] => b
			| sl::rest => let val t = all_except_option(s, sl) in
				if isSome t
				then aux(rest, b @ valOf t)
				else aux(rest, b)
				end
		in aux(sll, [])
		end

fun similar_names(sll : string list list, {first:string, last:string, middle:string}) =
	let val sl = get_substitutions2(sll, first)
	in
		let fun aux(t : string list) =
			case t of [] => []
			| str::rest => {first=str, last=last, middle=middle} :: aux(rest)
		in
			aux(first::sl)
		end
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
fun card_color c =
	case c of (st, rk) =>
		case st of Clubs => Black
			| Spades => Black
			| _ => Red

fun card_value c =
	case c of (st, rk) =>
		case rk of Num n => n
			| Ace => 11
			| _ => 10

fun remove_card(cs : card list, c : card, e) =
	let fun deal(done : card list, rest : card list) =
		case rest of now::r => if now = c then done @ r else deal(done @ now::[], r)
			| [] => raise e
	in
		deal([], cs)
	end
 
 fun all_same_color(cs : card list) =
 	let fun compare_c(c1 : card, c2 : card) = card_color(c1) = card_color(c2)
 	in
 		case cs of [] => true
 			| c::rest =>
 			case rest of [] => true
 				| c'::rest' => compare_c(c, c') andalso all_same_color(rest)
 	end

fun sum_cards(cs : card list) =
	let fun tail_sum(cs_sum : card list, ans : int) =
		case cs_sum of [] => ans
			| c::rest => tail_sum(rest, card_value(c) + ans)
	in 
		tail_sum(cs, 0)
	end 

fun score(cs : card list, goal : int) =
	let val simple_sum_ans = sum_cards(cs)
	in
		let val ans = if simple_sum_ans > goal then 3 * (simple_sum_ans - goal) else goal - simple_sum_ans
		in
			if all_same_color(cs) then ans div 2
			else ans
		end
	end

fun officiate(card_list : card list, move_list : move list, goal : int) =
	let fun deal(card_list_now : card list, move_list_now : move list, state : card list) =
		if (sum_cards(state) > goal) then score(state, goal) else 
		case move_list_now of [] => score(state, goal)
			| move_now::move_rest => 
			case move_now of Discard dc => deal(card_list_now, move_rest, remove_card(state, dc, IllegalMove))
				| Draw =>
				case card_list_now of [] => score(state, goal)
					| card::card_rest => deal(card_rest, move_rest, card::state)
	in
		deal(card_list, move_list, [])
	end
