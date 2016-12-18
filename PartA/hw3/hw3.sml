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

val only_capitals = List.filter(fn s => Char.isUpper(String.sub(s,0)))

val longest_string1 = foldl (fn(s1, s2) => if String.size s1 > String.size s2 then s1 else s2) ""

val longest_string2 = foldl (fn(s1, s2) => if String.size s1 >= String.size s2 then s1 else s2) ""

fun longest_string_helper f = foldl (fn(s1, s2) => if f(String.size s1, String.size s2) then s1 else s2) ""

val longest_string3 = longest_string_helper(fn(s1, s2) => s1 > s2)

val longest_string4 = longest_string_helper(fn(s1, s2) => s1 >= s2)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o rev o String.explode

fun first_answer f lst =
	case lst of
		[] => raise NoAnswer
	|	l::ls => case f l of
					NONE => first_answer f ls
				|	SOME v => v

fun all_answers f lst =
	let
		fun all_answers_helper lst acc = 
			case lst of
				[] => SOME acc
			|	l::ls => case f l of
							NONE => NONE
						|	SOME v => all_answers_helper ls (acc@v)
	in
		all_answers_helper lst []
	end

val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) (fn x => String.size x)

fun count_some_var(s, p) = g (fn _ => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p =
	let
		fun distinct lst =
			case lst of
				[] => true
			|	l::ls => not (List.exists (fn x => x = l) ls) andalso distinct ls
		fun extract pt =
			case pt of
				Variable x => [x]
            |	TupleP ps => List.foldl (fn (p1, p2) => p2 @ extract p1) [] ps
            |	ConstructorP (_, ps) => extract ps
            |	_ => []
	in
		distinct (extract p)
	end

fun match (v, p) =
	case (v, p) of
		(_, Wildcard) => SOME []
	|	(vs, Variable ps) => SOME [(ps, vs)]
	|	(Unit, UnitP) => SOME []
	|	(Const vs, ConstP ps) => if vs = ps then SOME [] else NONE
	|	(Tuple vs, TupleP ps) => if List.length vs <> List.length ps then NONE else all_answers match (ListPair.zip(vs, ps))
	|	(Constructor(v1, v2), ConstructorP(p1, p2)) => if v1 = p1 then match(v2, p2) else NONE
	|	_ => NONE

fun first_match v ps =
	SOME (first_answer(fn p => match(v, p)) ps)
    handle NoAnswer => NONE

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
