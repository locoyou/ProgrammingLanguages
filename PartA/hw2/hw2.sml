fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option(s, s_list) =
	case s_list of
			[] => NONE
		|	first::remains => if same_string(first, s) 
							then SOME remains 
							else case all_except_option(s, remains) of
									NONE => NONE
								|	SOME lst => SOME (first::lst)

fun get_substitutions1(lsts, s) =
	case lsts of
			[] => []
		|	first::remains => case all_except_option(s, first) of
									NONE => get_substitutions1(remains, s)
								|	SOME lst => lst @ get_substitutions1(remains, s)

fun get_substitutions2(lsts, s) =
	let
		fun get_substitutions_helper(lsts, s, acc) = 
			case lsts of
				[] => acc
			|	first::remains => case all_except_option(s, first) of
									NONE => get_substitutions_helper(remains, s, acc)
								|	SOME lst => get_substitutions_helper(remains, s, acc @ lst)
	in
		get_substitutions_helper(lsts, s, [])
	end
	
fun similar_names(lsts, {first=f, middle=m, last=l}) =
	let
		val substitutions = f::get_substitutions2(lsts, f)
		fun similar_names_helper(lst, acc) =
			case lst of
					[] => acc
				|	x::xs => similar_names_helper(xs, acc@[{first=x,middle=m,last=l}])
	in
		similar_names_helper(substitutions, [])
	end	

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

