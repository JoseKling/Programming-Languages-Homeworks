(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun all_except_option (str, str_list) = 
    case str_list of
          [] => NONE
        | head::tail => if same_string(head, str)
			then SOME (tail)
			else case all_except_option (str, tail) of
			      NONE => NONE
			    | SOME(ls) => SOME (head::ls)

fun get_substitutions1 (substitutions, name) = 
    case substitutions of
          [] => []
        | head::tail => case all_except_option(name, head) of
        		      NONE => get_substitutions1 (tail, name)
			    | SOME (ls) => ls @ get_substitutions1 (tail, name)

fun get_substitutions2 (substitutions, name) = 
    let fun helper(result, substitutions, name) =
		case substitutions of
		      [] => result
        	    | head::tail => case all_except_option(name, head) of
        	      		          NONE => helper (result, tail, name)
			    		| SOME (ls) => helper (result@ls, tail, name)
    in
	helper ([], substitutions, name)
    end

fun similar_names(substitutions, {first=first, middle=middle, last=last}) = 
    let fun substitute_name (subs, {first=first, middle=middle, last=last}) =
	    case subs of
		  [] => [{first=first, middle = middle, last = last}]
		| head::tail => {first=head, middle = middle, last = last}::substitute_name (tail, 						{first=first, middle=middle, last=last}) 	    
    in
	case get_substitutions2(substitutions, first) of
	      [] => []
	    | ls => substitute_name(ls, {first=first, middle=middle, last=last})
    end

fun card_color (card) = 
    case card of
          (Hearts, _) => Red
	| (Diamonds, _) => Red
	| (Clubs, _) => Black
	| (Spades, _) => Black

fun card_value (card) = 
    case card of
	  (_, Ace) => 11
	| (_, Num n) => n
	| _ => 10

fun remove_card (cs, c, e) = 
    case cs of
          [] => raise e
        | head::tail => if c = head
			then tail
			else head::remove_card(tail, c, e)

fun all_same_color(cards_list) = 
    case cards_list of
	  [] => true
	| head::[] => true
	| head::neck::body => if card_color(head) = card_color(neck)
			      then all_same_color(neck::body)
			      else false

fun sum_cards(cards_list) = 
    let fun helper(result, cards_list) = 
	    case cards_list of
		  [] => result
		| head::tail => helper(result + card_value(head), tail)
    in
	helper(0, cards_list)
    end

fun score(cards_list, goal) = 
    let
	val sum = sum_cards(cards_list)
	val score = if sum > goal
		    then 3*(sum - goal)
		    else (goal - sum)
    in
	if all_same_color(cards_list)
	then score div 2
	else score
    end

fun officiate(card_list, move_list, goal) = 
    let fun helper (card_list, move_list, hand) = 
	    case (move_list, card_list, hand) of
		  ([], _, ls) => score(ls, goal)
		| (Draw::moves, [], ls) => score(ls, goal)
		| (Draw::moves, first::others, ls) => if sum_cards(first::ls) > goal
						     then score(first::ls, goal)
						     else helper(others, moves, first::ls)
		| (Discard(card)::moves, cls, ls) => helper(cls, moves, remove_card(ls, card, IllegalMove))
    in
	helper(card_list, move_list, [])
    end