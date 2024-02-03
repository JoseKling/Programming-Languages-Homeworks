fun is_older(date1 : (int * int * int), date2 : (int * int * int) ) = 
	if (#1 date1) <> (#1 date2)
	then (#1 date1) < (#1 date2)
	else if (#2 date1) <> (#2 date2)
		then (#2 date1) < (#2 date2)
		else (#3 date1) < (#3 date2)

fun number_in_month(dates : (int * int * int) list, month : int) = 
	if null(dates)
	then 0
	else if (#2 (hd dates))=month
		then 1+number_in_month(tl dates, month)
		else number_in_month(tl dates, month)

fun number_in_months(dates : (int * int * int) list, months : int list) =
	if null(months)
	then 0
	else number_in_month(dates,hd months)+number_in_months(dates, tl months)

fun dates_in_month(dates : (int * int * int) list, month : int) = 
	if null(dates)
	then []
	else if (#2 (hd dates))=month
		then hd(dates) :: dates_in_month(tl dates, month)
		else dates_in_month(tl dates, month)

fun dates_in_months(dates : (int * int * int) list, months : int list) = 
	if null(months) orelse null(dates)
	then []
	else let 
		fun check_next(date : (int * int * int), months : int list) = 
			if null(months)
			then false
			else if (#2 date)=hd months
				then true
				else check_next(date, tl(months))
	      in
		if check_next(hd(dates), months)
		then hd(dates)::dates_in_months(tl(dates), months)
		else dates_in_months(tl(dates), months)
	      end

fun get_nth(strs : string list, n : int) = 
	if null(strs)
	then ""
	else 	if n=1
		then hd(strs)
		else get_nth(tl(strs), n-1)

fun date_to_string(date : (int * int * int)) = 
	let
		val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
		val month = get_nth(months, (#2 date))
	in
		 month^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
	end

fun number_before_reaching_sum(sum : int, lst : int list) = 
	if sum<=hd(lst) orelse null(lst)
	then 0
	else 1 + number_before_reaching_sum(sum-hd(lst), tl(lst))

fun what_month(day : int) = 
	let
		val day_in_months = [31, 29, 31, 30, 31, 30, 31, 30, 31, 30, 31, 30]
	in
		number_before_reaching_sum(day, day_in_months)+1
	end

fun month_range(day1 : int, day2 : int) = 
	if day1>day2
	then []
	else what_month(day1)::month_range(day1+1, day2)

fun oldest(dates : (int * int * int) list) = 
	if null(dates)
	then NONE
	else 	if null(tl(dates))
		then SOME(hd(dates))
		else	if is_older( hd(dates), hd(tl(dates)) )
			then oldest( hd(dates)::tl(tl(dates)) )
			else oldest( tl(dates) )

fun div_rem(nk : (int * int)) = 
	if (#2 nk)<(#1 nk)
	then ((#2 nk), 0)
	else	let
			val aux = div_rem((#1 nk), (#2 nk)-(#1 nk))
		in
			((#1 aux), 1+(#2 aux))
		end		

fun full_divide(nk : (int * int)) = 
	let
		val val_div = div_rem(nk)
	in
		if (#1 val_div)<>0
		then (0,#2 nk)
		else	let
				val aux = full_divide((#1 nk), (#2 val_div))
			in
				(1+(#1 aux), (#2 aux))	
			end
	end

fun factorize_from(k : int, n : int) =
	if k>n
	then []
	else 	let
			val res = full_divide((k,n))
		in
			if (#1 res)>0
			then (k,(#1 res))::factorize_from(k+2, (#2 res))
			else factorize_from(k+2, n)
		end

 fun factorize(n : int) = 
	let
		val res2 = full_divide((2,n))
	in
		if (#1 res2)=0
		then factorize_from(3, n)
		else (2,#1 res2)::factorize_from(3, n)
	end