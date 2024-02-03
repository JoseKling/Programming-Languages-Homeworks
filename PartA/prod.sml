(* FUnction that computes the product of a list of numbers *)

fun prod(ls : int list) = 
	if null(ls)
	then 1
	else hd(ls)*prod(tl(ls))