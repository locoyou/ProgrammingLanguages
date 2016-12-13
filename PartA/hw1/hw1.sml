fun is_older(first_date : int*int*int, second_date : int*int*int) =
    (#1 first_date < #1 second_date) orelse 
    (#1 first_date = #1 second_date andalso #2 first_date < #2 second_date) orelse
    (#1 first_date = #1 second_date andalso #2 first_date = #2 second_date andalso #3 first_date < #3 second_date)

fun number_in_month(dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else
        let
            val first_date_in_month = if #2 (hd dates) = month then 1 else 0
        in 
            first_date_in_month + number_in_month(tl dates, month)
        end 

fun number_in_months(dates : (int*int*int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else
    if #2 (hd dates) = month
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

fun dates_in_months(dates : (int*int*int) list, months : int list) =
    if null months
    then []
    else
        dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strings : string list, n : int) =
    if n = 1
    then hd strings
    else get_nth(tl strings, n-1)

fun date_to_string(year : int, month : int, day : int) = 
    let
        val months = ["January","February","March","April","May","June","July","August","September","October","November","December"]
    in
        get_nth(months, month) ^ " " ^ Int.toString(day) ^ ", " ^ Int.toString(year)
    end

fun number_before_reaching_sum(sum : int, numbers : int list) =
    if null numbers
    then 0
    else if (hd numbers) < sum
    then 1 + number_before_reaching_sum(sum - (hd numbers), tl numbers)
    else 0

fun what_month(day : int) =
    let
        val days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
        number_before_reaching_sum(day, days_in_month) + 1
    end

fun month_range(start_day : int, end_day : int) =
    if start_day > end_day
    then []
    else what_month(start_day) :: month_range(start_day + 1, end_day)

fun oldest(dates : (int*int*int) list) =
    if null dates
    then NONE
    else 
        let
            val tl_oldest = oldest(tl dates)
        in 
            if not (isSome(tl_oldest)) orelse is_older(hd dates, valOf(oldest(tl dates))) 
            then SOME(hd dates)
            else tl_oldest
        end

fun remove_duplicates(months : int list) =
    let
        fun duplicate(number : int, numbers : int list) =
            if (null numbers)
            then false
            else if (hd numbers) = number
            then true
            else duplicate(number, tl numbers)
    in
        if (null months) orelse (null (tl months))
        then months
        else if duplicate(hd months, tl months)
        then remove_duplicates(tl months)
        else (hd months) :: remove_duplicates(tl months)
    end

fun number_in_months_challenge(dates : (int*int*int) list, months : int list) =
    number_in_months(dates, remove_duplicates(months))

fun dates_in_months_challenge(dates : (int*int*int) list, months : int list) =
    dates_in_months(dates, remove_duplicates(months))

fun reasonable_date(year : int, month : int, day : int) =
    let
        val days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]
        val days_in_month_leap = [31,29,31,30,31,30,31,31,30,31,30,31]
        val is_leap = if (year mod 4 = 0 andalso year mod 100 <> 0) orelse (year mod 400 = 0) then true else false
        fun get_nth(days : int list, n : int) =
            if n = 1
            then hd days
            else get_nth(tl days, n-1)
    in
        if year <= 0 orelse month > 12 orelse month < 1
        then false
        else if is_leap
        then get_nth(days_in_month_leap, month) >= day andalso day > 0
        else get_nth(days_in_month, month) >= day andalso day > 0
    end