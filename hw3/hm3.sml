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

fun only_capitals sl =
	List.filter(fn x => Char.isUpper (String.sub (x,0))) sl

fun longest_string1 sl = 
	foldl(fn (x,y) => if String.size y < String.size x then x else y) "" sl

fun longest_string2 sl =
	foldl(fn (x,y) => if String.size y > String.size x then y else x) "" sl

fun longest_string_helper func =
	if func(2,1) then longest_string1 else longest_string2

val longest_string3 = longest_string_helper (fn (x,y) => x > y)

val longest_string4 = longest_string_helper (fn (x,y) => x < y)

fun longest_capitalized sl = 
	foldl(fn (x,y) => if Char.isUpper (String.sub (x,0)) andalso String.size y < String.size x then x else y) "" sl

fun rev_string str =
	String.implode(List.rev(String.explode str))

fun first_answer func xl =
	case xl of [] => raise NoAnswer
		| x::xl' => if isSome (func x) then valOf (func x) else first_answer func xl'

fun all_answers func xl = 
	let fun acc (func, xl, ans) =
		case xl of [] => SOME ans
			| x::xl' => if isSome(func x)
			then acc(func, xl', (valOf (func x))@ans)
			else NONE
	in acc(func, xl, []) end

fun count_wildcards p =
	g (fn _ => 1) (fn x => 1) p

fun count_wild_and_variable_lengths p =
	g (fn _ => 1) (fn x => String.size x) p

fun count_some_var (s, p) =
	g (fn _ => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p =
	let fun getStrList p =
		case p of Variable x => [x]
			| TupleP tp => List.foldl(fn (p,i) => (getStrList p) @ i) [] tp
			| ConstructorP (_,cp) => getStrList cp
			| _ => []
	in
		let fun filterString sl =
			case sl of [] => true
				| s::sl' => if List.exists (fn x => x = s) sl' then false else filterString sl'
				in
					filterString (getStrList p)
				end
	end

fun match (v, p) =
	case (v,p) of (_,Wildcard) => SOME []
		| (_, Variable s) => SOME [(s, v)]
		| (Unit, UnitP) => SOME []
		| (Const cv, ConstP cp) => if cv = cp then SOME [] else NONE
		| (Tuple vt, TupleP vp) =>
			if (List.length vt = List.length vp)
			then let val lp = ListPair.zip(vt, vp) in 
				all_answers match lp
			end
			else NONE
		| (Constructor(s1,coc), ConstructorP(s2,cop)) =>
			if s1 = s2
			then match (coc, cop)
			else NONE
		| _ => NONE

fun first_match v pl =
	let val lp = ListPair.zip([v], pl) in 
		SOME (first_answer match lp) handle NoAnswer => NONE
	end