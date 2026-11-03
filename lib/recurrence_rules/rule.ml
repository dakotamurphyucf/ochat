open Core

module Period = struct
  type t = int64 [@@deriving equal, sexp_of]

  let create_ms milliseconds =
    Result.map (Time_arithmetic.positive_ms_to_ns milliseconds) ~f:(fun _ -> milliseconds)
  ;;

  let milliseconds t = t
  let nanoseconds t = Int64.(t * 1_000_000L)
end

module Interval = struct
  type mode =
    | Fixed_rate
    | Fixed_delay
  [@@deriving equal, sexp]

  type t =
    { mode : mode
    ; period : Period.t
    }
  [@@deriving equal, sexp_of]

  let create ~mode ~period = { mode; period }
  let mode t = t.mode
  let period t = t.period
end

module Calendar = struct
  type pattern =
    | Daily
    | Weekdays of Day_of_week.t list
    | Month_days of int list
  [@@deriving equal, sexp]

  type gap =
    | Skip
    | At_gap_end
  [@@deriving equal, sexp]

  type repeat =
    | Earlier
    | Later
    | Both
  [@@deriving equal, sexp]

  type t =
    { pattern : pattern
    ; ofday : Time_ns.Ofday.t
    ; zone : string
    ; start_date : Date.t
    ; end_date : Date.t option
    ; gap : gap
    ; repeat : repeat
    }
  [@@deriving equal, sexp_of]

  let create
        ?(gap = Skip)
        ?(repeat = Earlier)
        ?end_date
        ~pattern
        ~ofday
        ~zone
        ~start_date
        ()
    =
    let open Result.Let_syntax in
    let%bind () = Zone.validate_name zone in
    let%bind _ = Time_arithmetic.local_scalar start_date ofday in
    let%bind () =
      match end_date with
      | None -> Ok ()
      | Some date ->
        let%bind _ = Time_arithmetic.local_scalar date ofday in
        if Date.compare start_date date <= 0
        then Ok ()
        else Codec.invalid "calendar dates must be ordered"
    in
    let%map pattern =
      match pattern with
      | Daily -> Ok Daily
      | Weekdays days ->
        if
          List.is_empty days
          || List.length days > 7
          || List.contains_dup days ~compare:Day_of_week.compare
        then Codec.invalid "weekdays must be a nonempty distinct set"
        else Ok (Weekdays (List.sort days ~compare:Day_of_week.compare))
      | Month_days days ->
        if
          List.is_empty days
          || List.length days > 31
          || List.contains_dup days ~compare:Int.compare
          || List.exists days ~f:(fun day -> day < 1 || day > 31)
        then Codec.invalid "month days must be a nonempty distinct set in 1..31"
        else Ok (Month_days (List.sort days ~compare:Int.compare))
    in
    { pattern; ofday; zone; start_date; end_date; gap; repeat }
  ;;

  let pattern t = t.pattern
  let ofday t = t.ofday
  let zone t = t.zone
  let start_date t = t.start_date
  let end_date t = t.end_date
  let gap t = t.gap
  let repeat t = t.repeat

  let matches t date =
    Date.compare date t.start_date >= 0
    && Option.value_map t.end_date ~default:true ~f:(fun last ->
      Date.compare date last <= 0)
    &&
    match t.pattern with
    | Daily -> true
    | Weekdays days -> List.mem days (Date.day_of_week date) ~equal:Day_of_week.equal
    | Month_days days -> List.mem days (Date.day date) ~equal:Int.equal
  ;;
end

type t =
  | Interval of Interval.t
  | Calendar of Calendar.t
[@@deriving equal, sexp_of]

let pattern_json = function
  | Calendar.Daily -> `Object [ "kind", `String "daily" ]
  | Weekdays days ->
    `Object
      [ "kind", `String "weekdays"
      ; ( "days"
        , `Array
            (List.map days ~f:(fun day ->
               Codec.number (Int64.of_int (Day_of_week.to_int day)))) )
      ]
  | Month_days days ->
    `Object
      [ "kind", `String "month_days"
      ; "days", `Array (List.map days ~f:(fun day -> Codec.number (Int64.of_int day)))
      ]
;;

let to_json = function
  | Interval rule ->
    `Object
      [ "version", Codec.number 1L
      ; "kind", `String "interval"
      ; ( "mode"
        , `String
            (match rule.mode with
             | Fixed_rate -> "fixed_rate"
             | Fixed_delay -> "fixed_delay") )
      ; "period_ms", Codec.number (Period.milliseconds rule.period)
      ]
  | Calendar rule ->
    `Object
      [ "version", Codec.number 1L
      ; "kind", `String "calendar"
      ; "pattern", pattern_json rule.pattern
      ; "ofday", `String (Time_ns.Ofday.to_string rule.ofday)
      ; "zone", `String rule.zone
      ; "start_date", `String (Date.to_string rule.start_date)
      ; ( "end_date"
        , Option.value_map rule.end_date ~default:`Null ~f:(fun date ->
            `String (Date.to_string date)) )
      ; ( "gap"
        , `String
            (match rule.gap with
             | Skip -> "skip"
             | At_gap_end -> "at_gap_end") )
      ; ( "repeat"
        , `String
            (match rule.repeat with
             | Earlier -> "earlier"
             | Later -> "later"
             | Both -> "both") )
      ]
;;

let pattern_of_json json =
  let open Result.Let_syntax in
  match json with
  | `Object fields ->
    let%bind kind_json = Codec.field fields "kind" in
    let%bind kind = Codec.string kind_json in
    let%bind fields =
      Codec.object_fields
        json
        ~required:(if String.equal kind "daily" then [ "kind" ] else [ "kind"; "days" ])
    in
    (match kind with
     | "daily" -> Ok Calendar.Daily
     | "weekdays" ->
       let%bind encoded = Codec.field fields "days" in
       let%map days =
         Codec.list
           (fun json ->
              let%bind day = Codec.int json in
              Day_of_week.of_int day
              |> Result.of_option ~error:(Timing_error.Invalid "weekday must be in 0..6"))
           encoded
       in
       Calendar.Weekdays days
     | "month_days" ->
       let%bind encoded = Codec.field fields "days" in
       let%map days = Codec.list Codec.int encoded in
       Calendar.Month_days days
     | _ -> Codec.invalid "unknown calendar pattern")
  | _ -> Codec.invalid "expected calendar pattern object"
;;

let of_json json =
  let open Result.Let_syntax in
  match json with
  | `Object initial ->
    let%bind kind_json = Codec.field initial "kind" in
    let%bind kind = Codec.string kind_json in
    (match kind with
     | "interval" ->
       let%bind fields =
         Codec.object_fields json ~required:[ "version"; "kind"; "mode"; "period_ms" ]
       in
       let%bind () = Codec.version fields in
       let%bind mode_json = Codec.field fields "mode" in
       let%bind mode =
         match mode_json with
         | `String "fixed_rate" -> Ok Interval.Fixed_rate
         | `String "fixed_delay" -> Ok Interval.Fixed_delay
         | _ -> Codec.invalid "invalid interval mode"
       in
       let%bind period_json = Codec.field fields "period_ms" in
       let%bind milliseconds = Codec.int64 period_json in
       let%map period = Period.create_ms milliseconds in
       Interval (Interval.create ~mode ~period)
     | "calendar" ->
       let%bind fields =
         Codec.object_fields
           json
           ~required:
             [ "version"
             ; "kind"
             ; "pattern"
             ; "ofday"
             ; "zone"
             ; "start_date"
             ; "end_date"
             ; "gap"
             ; "repeat"
             ]
       in
       let%bind () = Codec.version fields in
       let%bind pattern_json = Codec.field fields "pattern" in
       let%bind pattern = pattern_of_json pattern_json in
       let%bind ofday_json = Codec.field fields "ofday" in
       let%bind ofday = Codec.ofday ofday_json in
       let%bind zone_json = Codec.field fields "zone" in
       let%bind zone = Codec.string zone_json in
       let%bind start_json = Codec.field fields "start_date" in
       let%bind start_date = Codec.date start_json in
       let%bind end_json = Codec.field fields "end_date" in
       let%bind end_date = Codec.nullable Codec.date end_json in
       let%bind gap_json = Codec.field fields "gap" in
       let%bind gap =
         match gap_json with
         | `String "skip" -> Ok Calendar.Skip
         | `String "at_gap_end" -> Ok Calendar.At_gap_end
         | _ -> Codec.invalid "invalid gap policy"
       in
       let%bind repeat_json = Codec.field fields "repeat" in
       let%bind repeat =
         match repeat_json with
         | `String "earlier" -> Ok Calendar.Earlier
         | `String "later" -> Ok Calendar.Later
         | `String "both" -> Ok Calendar.Both
         | _ -> Codec.invalid "invalid repeat policy"
       in
       let%map rule =
         Calendar.create ~gap ~repeat ?end_date ~pattern ~ofday ~zone ~start_date ()
       in
       Calendar rule
     | _ -> Codec.invalid "unknown recurrence rule kind")
  | _ -> Codec.invalid "expected recurrence rule object"
;;
