open Core

let nanoseconds t = Time_ns.to_int63_ns_since_epoch t |> Int63.to_int64

let of_nanoseconds ns =
  if
    Int64.(ns < Int63.to_int64 Int63.min_value)
    || Int64.(ns > Int63.to_int64 Int63.max_value)
  then Error (Timing_error.Out_of_range "timestamp endpoint")
  else Ok (Time_ns.of_int63_ns_since_epoch (Int63.of_int64_exn ns))
;;

let add_ns time amount =
  let ns = nanoseconds time in
  let lower = Int64.(Int63.to_int64 Int63.min_value - ns) in
  let upper = Int64.(Int63.to_int64 Int63.max_value - ns) in
  if Int64.(amount < lower || amount > upper)
  then Error (Timing_error.Out_of_range "timestamp addition")
  else of_nanoseconds Int64.(ns + amount)
;;

let positive_ms_to_ns milliseconds =
  if Int64.(milliseconds <= 0L || milliseconds > max_value / 1_000_000L)
  then Error (Timing_error.Invalid "period must be positive representable milliseconds")
  else Ok Int64.(milliseconds * 1_000_000L)
;;

let add_ms time milliseconds =
  let open Result.Let_syntax in
  let%bind amount = positive_ms_to_ns milliseconds in
  add_ns time amount
;;

let local_scalar date ofday =
  if Time_ns.Ofday.compare ofday Time_ns.Ofday.start_of_next_day >= 0
  then Error (Timing_error.Invalid "local ofday must precede 24:00")
  else (
    let days = Date.diff date Date.unix_epoch |> Int64.of_int in
    let day_ns = 86_400_000_000_000L in
    (* Admit the date before multiplying: Date's range is much wider than either
       Int63 or Int64 nanoseconds. These coarse day bounds make the subsequent
       Int64 multiplication/addition safe; of_nanoseconds checks the exact edge. *)
    if
      Int64.(days < (Int63.to_int64 Int63.min_value / day_ns) - 1L)
      || Int64.(days > Int63.to_int64 Int63.max_value / day_ns)
    then Error (Timing_error.Out_of_range "civil date is outside Time_ns range")
    else (
      let ofday_ns =
        Time_ns.Ofday.to_span_since_start_of_day ofday
        |> Time_ns.Span.to_int63_ns
        |> Int63.to_int64
      in
      of_nanoseconds Int64.((days * day_ns) + ofday_ns)))
;;

let utc_parts time =
  let ns = nanoseconds time in
  let day_ns = 86_400_000_000_000L in
  let quotient = Int64.(ns / day_ns) in
  (* Core's [%] is Euclidean, while integer division truncates toward zero.
     Compute the signed remainder explicitly before correcting to floor days. *)
  let signed_remainder = Int64.(ns - (quotient * day_ns)) in
  let days = if Int64.(signed_remainder < 0L) then Int64.pred quotient else quotient in
  let remainder = Int64.(ns - (days * day_ns)) in
  let date = Date.add_days Date.unix_epoch (Int64.to_int_exn days) in
  let ofday =
    Int63.of_int64_exn remainder
    |> Time_ns.Span.of_int63_ns
    |> Time_ns.Ofday.of_span_since_start_of_day_exn
  in
  date, ofday
;;

let day_bounds_ns date =
  let first, _ = utc_parts (Time_ns.of_int63_ns_since_epoch Int63.min_value) in
  let last, _ = utc_parts (Time_ns.of_int63_ns_since_epoch Int63.max_value) in
  if Date.compare date first < 0 || Date.compare date (Date.add_days last 1) > 0
  then Error (Timing_error.Out_of_range "civil cursor date")
  else (
    (* At most one day beyond a signed Int63 endpoint; both raw day boundaries
       fit Int64 even when midnight is not a representable Time_ns value. *)
    let day_ns = 86_400_000_000_000L in
    let lower = Int64.(of_int (Date.diff date Date.unix_epoch) * day_ns) in
    Ok (lower, Int64.(lower + day_ns)))
;;

let to_json time = `String (Time_ns.to_string_utc time)

let of_json = function
  | `String encoded
    when String.length encoded >= 20
         && String.length encoded <= 30
         && String.is_suffix encoded ~suffix:"Z"
         && (Char.equal encoded.[10] 'T' || Char.equal encoded.[10] ' ') ->
    let date_string = String.sub encoded ~pos:0 ~len:10 in
    let ofday_string = String.sub encoded ~pos:11 ~len:(String.length encoded - 12) in
    let valid_ofday_shape =
      String.length ofday_string >= 8
      && Char.equal ofday_string.[2] ':'
      && Char.equal ofday_string.[5] ':'
      && String.for_alli ofday_string ~f:(fun i c ->
        if Int.equal i 2 || Int.equal i 5
        then true
        else if Int.equal i 8
        then Char.equal c '.'
        else Char.is_digit c)
      && String.length ofday_string <> 9
      && Int.of_string (String.sub ofday_string ~pos:0 ~len:2) < 24
      && Int.of_string (String.sub ofday_string ~pos:3 ~len:2) < 60
      && Int.of_string (String.sub ofday_string ~pos:6 ~len:2) < 60
    in
    (match
       Or_error.try_with (fun () ->
         Date.of_string date_string, Time_ns.Ofday.of_string ofday_string)
     with
     | Ok (date, ofday)
       when valid_ofday_shape && String.equal (Date.to_string date) date_string ->
       local_scalar date ofday
     | Ok _ | Error _ -> Error (Timing_error.Invalid "invalid UTC timestamp"))
  | _ -> Error (Timing_error.Invalid "timestamp must have explicit UTC Z suffix")
;;
