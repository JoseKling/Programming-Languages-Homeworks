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

val only_capitals = List.filter (fn str => Char.isUpper(String.sub(str, 0)))

val longest_string1 = foldl (fn (str1, str2) => if String.size(str1) > String.size(str2)
				  then str1
				  else str2) (* frist argument for fold *)
	      		    ""               (* second argument for fold *)

val longest_string2 = foldl (fn (str1, str2) => if String.size(str1) >= String.size(str2)
				  then str1
				  else str2) (* frist argument for fold *)
	      		    ""               (* second argument for fold *)

fun longest_string_helper f str_list =  
	case str_list of
	  [] => ""
	| head::[] => head
	| head::neck::tail => let val len1 = String.size(head)
				  val len2 = String.size(neck)
			      in
				  if (f (len2, len1)) 
			          then longest_string_helper f (neck::tail)
			          else longest_string_helper f (head::tail)
			      end

val longest_string3 = longest_string_helper (fn (int1, int2) => int1 > int2)

val longest_string4 = longest_string_helper (fn (int1, int2) => int1 >= int2)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o rev o String.explode

fun first_answer f a_list = 
	case a_list of
	  [] => raise NoAnswer
	| head::tail => case f head of
			  SOME value => value
			| NONE => first_answer f tail

fun all_answers f a_list =
    let fun helper func b_list acc = 
	case b_list of
	  [] => SOME (acc)
	| head::tail => case (func head) of
			  NONE => NONE
			| SOME value => SOME ((valOf (helper func tail (value @ acc))))
    in
	helper f a_list []
    end
		  
val count_wildcards = g (fn () => 1) (fn str => 0) 
	
val count_wild_and_variable_lengths = g (fn () => 1) (fn str => String.size(str))

fun count_some_var((str, pat)) = 
	let fun f str2 = if str = str2
			 then 1
			 else 0
	in
		g (fn () => 0) (f) (pat)
	end
	
fun check_pat(pat) = 
    let fun helper1 acc patt = 
	case patt of Variable str => str::acc 
	    | ConstructorP (str, pattern) => helper1 acc pattern
	    | TupleP p_list => foldl (fn (patt, str_l) => helper1 str_l patt) acc p_list
	    | _ => acc
	fun helper2(str_list) = case str_list of
	      [] => true
	    | head::tail => not (List.exists (fn str => str=head) tail) andalso helper2(tail)
	in
		helper2(helper1 [] pat)
	end

fun match (value, pat) = 
    case (value, pat) of
	  (Unit, UnitP) => SOME []
	| ((Const x), (ConstP y)) => if x=y then (SOME []) else NONE
	| (v, Variable str) => SOME [(str, v)]
	| (_, Wildcard) => SOME []
	| (Constructor(s2, v), ConstructorP(s1, p)) => if s1=s2 then match (v, p) else NONE
	| (Tuple vs, TupleP ps) => if List.length(vs) = List.length(ps)
				   then all_answers match (ListPair.zip (vs, ps))
				   else NONE
	| _ => NONE

fun first_match value pat_list = 
    let fun match_helper value pat = match (value, pat) 
    in
	SOME (first_answer (match_helper value) pat_list)
		handle NoAnswer => NONE
    end
	
	