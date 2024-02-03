(* THis is how to make function *)

fun factorial (x : int) = 
	if x<1	then 0
	else if x=1 then 1
	else x * factorial(x - 1)