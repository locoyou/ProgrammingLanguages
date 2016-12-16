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

fun card_color(s, r) =
	case s of
		Clubs => Black
	|	Spades => Black
	|	_ => Red

fun card_value(s, r) =
	case r of
		Ace => 11
	|	Num x => x
	|	_ => 10

fun remove_card(cards, c, e) =
	let
		fun remove_card_helper(cards, c, acc) =
			case cards of
				[] => raise e
			|	x::xs => if x=c then acc@xs else remove_card_helper(xs, c, acc@[x])
	in
		remove_card_helper(cards, c, [])
	end

fun all_same_color(cards) =
	case cards of
		[] => true
	|	x::[] => true
	|	x::y::z => card_color(x) = card_color(y) andalso all_same_color(y::z)

fun sum_cards(cards) =
	let
		fun sum_cards_helper(cards, acc) = 
			case cards of
				[] => acc
			|	x::xs => sum_cards_helper(xs, acc + card_value(x))
	in
		sum_cards_helper(cards, 0)
	end

fun score(cards, goal) =
	let
		val sum = sum_cards(cards)
		val same_color = all_same_color(cards)
		val p = if sum > goal then 3 * (sum - goal) else goal - sum
	in
		if same_color then p div 2 else p
	end

fun officiate(cards, moves, goal) =
	let
		fun officiate_helper(cards, moves, held_cards, sum) = 
			let 
				val current_score = score(held_cards, goal)
			in
				if sum > goal
				then current_score
				else case moves of
						[] => current_score
					|	(Discard c)::ms => officiate_helper(cards, ms, remove_card(held_cards, c, IllegalMove), sum - card_value(c))
					|	Draw::ms =>	case cards of
									[] => current_score
								|	c::cs => officiate_helper(cs, ms, c::held_cards, sum + card_value(c))
			end
	in
		officiate_helper(cards, moves, [], 0)
	end

fun small_card_value(s, r) =
	case r of
		Ace => 1
	|	Num x => x
	|	_ => 10

fun small_sum_cards(cards) =
	let
		fun sum_cards_helper(cards, acc) = 
			case cards of
				[] => acc
			|	x::xs => sum_cards_helper(xs, acc + small_card_value(x))
	in
		sum_cards_helper(cards, 0)
	end

fun score_challenge(cards, goal) =
	let
		val big_sum = sum_cards(cards)
		val small_sum = small_sum_cards(cards)
		val same_color = all_same_color(cards)
		fun pscore(sum) =
			if sum > goal then 3 * (sum - goal) else goal - sum
		fun score_challenge_helper(sum) =
			if small_sum >= sum orelse goal >= sum then pscore(sum) else Int.min(pscore(sum), score_challenge_helper(sum - 10))
	in
		if same_color then score_challenge_helper(big_sum) div 2 else score_challenge_helper(big_sum)
	end

fun officiate_challenge(cards, moves, goal) =
	let
		fun officiate_helper(cards, moves, held_cards, sum) = 
			let 
				val current_score = score_challenge(held_cards, goal)
			in
				if sum > goal
				then current_score
				else case moves of
						[] => current_score
					|	(Discard c)::ms => officiate_helper(cards, ms, remove_card(held_cards, c, IllegalMove), sum - small_card_value(c))
					|	Draw::ms =>	case cards of
									[] => current_score
								|	c::cs => officiate_helper(cs, ms, c::held_cards, sum + small_card_value(c))
			end
	in
		officiate_helper(cards, moves, [], 0)
	end

fun careful_player(cards, goal) =
	let
		fun find(cards, value) =
			case cards of
				[] => NONE
			|	x::xs => if card_value(x) = value then SOME x else find(xs, value)
		fun careful_player_helper(cards, held_cards, acc, sum) =
			if goal - sum > 10 
			then case cards of
				[] => acc@[Draw]
			|	x::xs => careful_player_helper(xs, x::held_cards, acc@[Draw], sum+card_value(x))
			else if sum = goal
			then acc
			else case cards of
				[] => acc
			|	x::xs => case find(held_cards, sum + card_value(x) - goal) of
						 	NONE => acc
						 |  SOME c => acc@[Discard c, Draw]

		
	in
		careful_player_helper(cards, [], [], 0)
	end