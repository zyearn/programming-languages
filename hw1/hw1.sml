fun is_older(x: int*int*int, y:int*int*int) = 
  if #1 x < #1 y
  then true
  else if #1 x > #1 y
  then false
  else if #2 x < #2 y
  then true
  else if #2 x > #2 y
  then false
  else if #3 x < #3 y
  then true
  else false

fun is_older_better(date1 : int * int * int, date2 : int * int * int) =
    let 
        val y1 = #1 date1
        val m1 = #2 date1
        val d1 = #3 date1
        val y2 = #1 date2
        val m2 = #2 date2
        val d2 = #3 date2
    in
        y1 < y2 orelse (y1=y2 andalso m1 < m2)
                orelse (y1=y2 andalso m1=m2 andalso d1 < d2)
    end 

fun number_in_month(x: (int*int*int) list, y: int) = 
  if null x
  then 0
  else if #2 (hd x) = y
  then 1 + number_in_month(tl x, y)
  else number_in_month(tl x, y)

fun number_in_months(x: (int*int*int) list, y: int list) =
  if null y
  then 0
  else number_in_month(x, hd y) + number_in_months(x, tl y)

fun dates_in_month(x: (int*int*int) list, y: int) = 
  if null x
  then []
  else if #2 (hd x) = y
  then (hd x) :: dates_in_month(tl x, y)
  else dates_in_month(tl x, y)

fun dates_in_months(x: (int*int*int) list, y: int list) = 
  if null y
  then []
  else dates_in_month(x, hd y) @ dates_in_months(x, tl y)

fun get_nth(x: string list, n: int) = 
  if n = 1
  then hd x
  else get_nth(tl x, n-1)

fun date_to_string(x: int*int*int) = 
  let 
    val ms = ["January", "February", "March", "April", "May", "June", "July",
  "August", "September", "October", "November", "December"]
  in
    get_nth(ms, #2 x) ^ " " ^ Int.toString(#3 x) ^ ", " ^ Int.toString(#1 x)
  end

fun number_before_reaching_sum(x: int, y: int list) = 
  if null y
  then 0
  else if x > (hd y)
  then 1 + number_before_reaching_sum(x - (hd y), tl y)
  else 0

fun what_month(x: int) = 
  let 
    val ds = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum(x, ds) + 1
  end

fun month_range(x: int, y: int) = 
  if x > y
  then []
  else what_month(x) :: month_range(x+1, y)

fun oldest(x: (int*int*int) list) = 
  if null x
  then NONE
  else
    let
      fun oldest_nonempty(x: (int*int*int) list) = 
        if null (tl x)
        then hd x
        else
          let val ans = oldest_nonempty(tl x)
          in
            if is_older((hd x), ans)
            then hd x
            else ans
          end
    in
        SOME (oldest_nonempty(x))
    end

fun number_in_months_challenge(x: (int*int*int) list, y: int list) = 
  let
    fun remove_dup(z: int list) = 
      if null z
      then []
      else
        let
          fun inlist(x: int, l: int list) = 
            if null l
            then false
            else if (hd l) = x
            then true
            else inlist(x, (tl l))
          val ans = remove_dup(tl z)
        in
          if inlist((hd z), ans)
          then ans
          else (hd z)::ans
        end
  in
    number_in_months(x, remove_dup(y))
  end

fun reasonable_date(x: (int*int*int)) = 
  let
    fun isLeap(x: int) = 
      if (x mod 400) = 0 
      then true
      else if (x mod 4) = 0 andalso (x mod 100 <> 0)
      then true
      else false
    fun get_nth(x: int list, n: int) = 
      if n = 1
      then hd x
      else get_nth(tl x, n-1)
    val my = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    val ly = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    val y = #1 x
    val m = #2 x
    val d = #3 x
    val isl = isLeap(y)
  in
    if m < 1 orelse m > 12
    then false
    else if isl
    then d <= get_nth(ly, m) andalso d > 0
    else d <= get_nth(my, m) andalso d > 0
  end
