open Core

(* Independent integer Gregorian oracle: no Date rule construction, production
   parser, transition sorter or400-year reduction. Explicit seasonal intervals
   are tested by membership, rather than replaying production change events. *)
type day =
  | Julian of int
  | Ordinal of int
  | Month of int * int * int

type rule =
  { day : day
  ; seconds : int
  }

type t =
  { standard : int
  ; daylight : int
  ; start : rule
  ; finish : rule
  }

let leap year = year % 4 = 0 && (year % 100 <> 0 || year % 400 = 0)
let year_days year = if leap year then 366 else 365

let month_days year =
  [ 31; (if leap year then 29 else 28); 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 ]
;;

let first_day year =
  if year >= 1970
  then List.range 1970 year |> List.sum (module Int) ~f:year_days
  else -(List.range year 1970 |> List.sum (module Int) ~f:year_days)
;;

let ordinal day year =
  match day with
  | Julian n -> n - 1 + if leap year && n >= 60 then 1 else 0
  | Ordinal n -> n
  | Month (month, week, weekday) ->
    let lengths = month_days year in
    let prior = List.take lengths (month - 1) |> List.sum (module Int) ~f:Fn.id in
    let length = List.nth_exn lengths (month - 1) in
    let matching =
      List.range 0 length
      |> List.filter ~f:(fun day -> (first_day year + prior + day + 4) % 7 = weekday)
    in
    let day =
      if week = 5 then List.last_exn matching else List.nth_exn matching (week - 1)
    in
    prior + day
;;

let at rule year offset =
  let days = first_day year + ordinal rule.day year in
  Int64.((of_int days * 86400L) + of_int rule.seconds - of_int offset)
;;

let daylight t ~year ~at:probe =
  List.exists
    [ year - 1; year; year + 1 ]
    ~f:(fun year ->
      let start = at t.start year t.standard in
      let finish = at t.finish year t.daylight in
      let finish =
        if Int64.(finish < start) then at t.finish (year + 1) t.daylight else finish
      in
      Int64.(probe >= start && probe < finish))
;;

let offset t ~year ~at = if daylight t ~year ~at then t.daylight else t.standard
