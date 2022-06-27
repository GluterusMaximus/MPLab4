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


(**** you can put all your code here ****)

fun only_capitals (strings : string list) =
  List.filter(fn str : string => Char.isUpper(String.sub(str, 0))) strings

fun longest_string1 (strings : string list) =
  List.foldl (fn (str, longest) => if String.size(str) > String.size(longest) then str else longest) "" strings

fun longest_string2 (strings : string list) =
  List.foldl (fn (str, longest) => if String.size(str) >= String.size(longest) then str else longest) "" strings

fun longest_string_helper f = fn (strings : string list) =>
  List.foldl (fn (str, longest) => 
    if  f (String.size(str), String.size(longest)) 
    then str else longest) "" strings

val longest_string3 = longest_string_helper(fn (size1, size2) => size1 > size2)

val longest_string4 = longest_string_helper(fn (size1, size2) => size1 >= size2)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f list =
  case list of
    [] => raise NoAnswer
    | x::xs' => case f(x) of
                SOME y => y
                | _ => first_answer f xs'

fun all_answers f list =
  let fun helper([], acc) = SOME(acc)
    | helper (x::xs', acc) =
        case f(x) of
        SOME x => helper(xs', acc@x)
        | NONE => NONE
  in helper(list, []) end

val count_wildcards = g (fn () => 1) (fn (x) => 0)

val count_wild_and_variable_lengths = g (fn () => 1) String.size

fun count_some_var (var:string, p) = g (fn () => 0) (fn (x) => if x = var then 1 else 0) p

fun pattern_vars p =
  case p of
	    Wildcard          => []
	  | Variable x        => [x]
	  | TupleP ps         => List.foldl (fn (p,i) => (pattern_vars p) @ i) [] ps
	  | ConstructorP(_,p) => pattern_vars p
	  | _                 => []

fun has_repeats [] = false
| has_repeats (x::xs) = 
  if List.exists (fn (i) => i = x) xs then true else has_repeats xs;

fun check_pat p = not (has_repeats(pattern_vars p))

fun match (valu, pattern) =
  case (valu, pattern) of
	    (_, Wildcard)           => SOME []
	  | (v, Variable s)         => SOME [(s, v)]
    | (Unit, UnitP)           => SOME []
    | (Const c1, ConstP c2)   => if (c1 = c2) then SOME([]) else NONE
	  | (Tuple vs, TupleP ps)   => if length vs = length ps
                                 then all_answers match (ListPair.zip(vs, ps))
                                 else NONE
	  | (Constructor(s1, v), ConstructorP(s2, p)) => 
                                 if s1 = s2
                                 then match(v,p)
                                 else NONE
	  | (_, _)                  => NONE

fun first_match value patterns =
  SOME (first_answer (fn x => match(value, x)) patterns)
  handle NoAnswer => NONE
