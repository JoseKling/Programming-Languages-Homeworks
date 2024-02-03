fun countdown(x : int) = 
	if x<1 
	then []
	else x::countdown(x-1)

fun countup(x : int) = 
	if x=1
	then 1::[]
	else countup(x+1)

fun append(ls1 : 'a list, ls2 : 'a list) =
	if null(ls1)
	then ls2
	else hd(ls1)::append(tl(ls1), ls2)
	
fun smalls(ls : (int * int) list) = 
	if null(ls)
	then []
	else if (#1 (hd ls))>(#2 (hd ls))
		then (#2 (hd ls))::smalls(tl ls)
		else (#1 (hd ls))::smalls(tl ls)

fun firsts(ls : (int * int) list) = 
	if null(ls)
	then []
	else (#1 (hd ls))::firsts(tl ls)