open Core
module Error = Zone_data_error
module Local_type = Tzif_reader.Local_type
module Transition = Tzif_reader.Transition

module Day = struct
  type t =
    | Julian of int
    | Ordinal of int
    | Month_week_day of
        { month : int
        ; week : int
        ; weekday : int
        }
end

type rule =
  { day : Day.t
  ; seconds : int
  }

type parsed =
  | Fixed of Local_type.t
  | Seasonal of
      { standard : Local_type.t
      ; daylight : Local_type.t
      ; start : rule
      ; finish : rule
      }

exception Rejected of Error.t

module Parser = struct
  type t =
    { text : string
    ; mutable position : int
    }

  let reject reason = raise (Rejected (Unsupported_footer reason))
  let peek t = String.get t.text t.position
  let ended t = t.position = String.length t.text
  let current t = if ended t then None else Some (peek t)
  let advance t = t.position <- t.position + 1

  let expect t expected =
    match current t with
    | Some actual when Char.equal expected actual -> advance t
    | Some _ | None -> reject "invalid POSIX TZ separator"
  ;;

  let number t ~maximum =
    let start = t.position in
    let value = ref 0 in
    while (not (ended t)) && Char.is_digit (peek t) do
      let digit = Char.to_int (peek t) - Char.to_int '0' in
      if !value > (maximum - digit) / 10 || digit > maximum
      then reject "POSIX TZ number out of range";
      value := (!value * 10) + digit;
      advance t
    done;
    if t.position = start then reject "missing POSIX TZ number";
    !value
  ;;

  let name t =
    let quoted =
      match current t with
      | Some '<' ->
        advance t;
        true
      | Some _ | None -> false
    in
    let start = t.position in
    let allowed char =
      Char.is_alpha char
      || (quoted && (Char.is_digit char || Char.equal char '+' || Char.equal char '-'))
    in
    while (not (ended t)) && allowed (peek t) do
      advance t
    done;
    let length = t.position - start in
    if length < 3 then reject "POSIX TZ designation must have at least three characters";
    let value = String.sub t.text ~pos:start ~len:length in
    if quoted then expect t '>';
    value
  ;;

  let seconds t ~hours ~signed =
    let sign =
      match current t with
      | Some '-' when signed ->
        advance t;
        -1
      | Some '+' when signed ->
        advance t;
        1
      | Some _ | None -> 1
    in
    let hour = number t ~maximum:hours in
    let minute, second =
      match current t with
      | Some ':' ->
        advance t;
        let minute = number t ~maximum:59 in
        let second =
          match current t with
          | Some ':' ->
            advance t;
            number t ~maximum:59
          | Some _ | None -> 0
        in
        minute, second
      | Some _ | None -> 0, 0
    in
    sign * ((hour * 3600) + (minute * 60) + second)
  ;;

  let rule t ~version =
    let day =
      match current t with
      | Some 'J' ->
        advance t;
        let day = number t ~maximum:365 in
        if day = 0 then reject "Julian rule day must be positive";
        Day.Julian day
      | Some 'M' ->
        advance t;
        let month = number t ~maximum:12 in
        expect t '.';
        let week = number t ~maximum:5 in
        expect t '.';
        let weekday = number t ~maximum:6 in
        if month = 0 || week = 0 then reject "month and week must be positive";
        Month_week_day { month; week; weekday }
      | Some _ | None -> Ordinal (number t ~maximum:365)
    in
    let seconds =
      match current t with
      | Some '/' ->
        advance t;
        (match version with
         | Tzif_reader.Version.V2 -> seconds t ~hours:24 ~signed:false
         | V3 -> seconds t ~hours:167 ~signed:true)
      | Some _ | None -> 7200
    in
    { day; seconds }
  ;;
end

let local_type ~designation ~offset_seconds ~is_dst =
  if offset_seconds < -86400 || offset_seconds > 86400
  then raise (Rejected (Unsupported_offset offset_seconds));
  { Local_type.designation; offset_seconds; is_dst }
;;

let parse text ~version =
  if String.is_empty text
  then Ok None
  else if String.length text > Tzif_reader.maximum_footer_bytes
  then Error (Error.Unsupported_footer "footer resource bound")
  else (
    let parser = { Parser.text; position = 0 } in
    try
      let designation = Parser.name parser in
      let offset_seconds = -Parser.seconds parser ~hours:24 ~signed:true in
      let standard = local_type ~designation ~offset_seconds ~is_dst:false in
      let result =
        if Parser.ended parser
        then Fixed standard
        else (
          let designation = Parser.name parser in
          let offset_seconds =
            match Parser.current parser with
            | Some ',' | None -> standard.offset_seconds + 3600
            | Some _ -> -Parser.seconds parser ~hours:24 ~signed:true
          in
          let daylight = local_type ~designation ~offset_seconds ~is_dst:true in
          if Parser.ended parser
          then Parser.reject "DST rules require explicit start and end";
          Parser.expect parser ',';
          let start = Parser.rule parser ~version in
          Parser.expect parser ',';
          let finish = Parser.rule parser ~version in
          if not (Parser.ended parser) then Parser.reject "trailing POSIX TZ data";
          Seasonal { standard; daylight; start; finish })
      in
      Ok (Some result)
    with
    | Rejected error -> Error error)
;;

let day_date day ~year =
  let january = Date.create_exn ~y:year ~m:Month.Jan ~d:1 in
  match day with
  | Day.Julian day ->
    let leap = Date.days_in_month ~year ~month:Month.Feb = 29 in
    Date.add_days january (day - 1 + if leap && day >= 60 then 1 else 0)
  | Ordinal day -> Date.add_days january day
  | Month_week_day { month; week; weekday } ->
    let month = Month.of_int_exn month in
    let first = Date.create_exn ~y:year ~m:month ~d:1 in
    let first_weekday = Date.day_of_week first |> Day_of_week.to_int in
    let first_match = 1 + ((weekday - first_weekday + 7) % 7) in
    let candidate = first_match + ((week - 1) * 7) in
    let last = Date.days_in_month ~year ~month in
    let day = if candidate > last then candidate - 7 else candidate in
    Date.create_exn ~y:year ~m:month ~d:day
;;

let instant rule ~year ~previous_offset =
  let date = day_date rule.day ~year in
  Int64.(
    (of_int (Date.diff date Date.unix_epoch) * 86400L)
    + of_int rule.seconds
    - of_int previous_offset)
;;

type event =
  { at : int64
  ; year : int
  ; start : bool
  ; local_type : Local_type.t
  }

let cycle = 12_622_780_800L
let epoch = 946_684_800L

let phase at =
  (* Reducing operands before subtraction avoids signed64-bit overflow. *)
  let value = Int64.(rem (rem at cycle - rem epoch cycle) cycle) in
  if Int64.(value < 0L) then Int64.(value + cycle) else value
;;

let ordered events =
  List.sort events ~compare:(fun a b ->
    match Int64.compare a.at b.at with
    | 0 ->
      (match Int.compare a.year b.year with
       | 0 -> Bool.compare b.start a.start
       | order -> order)
    | order -> order)
  |> List.fold ~init:[] ~f:(fun acc event ->
    match acc with
    | previous :: rest when Int64.equal event.at previous.at -> event :: rest
    | [] | _ :: _ -> event :: acc)
  |> List.rev
;;

let annual_events parsed year =
  match parsed with
  | Fixed _ -> []
  | Seasonal { standard; daylight; start; finish } ->
    let start = instant start ~year ~previous_offset:standard.offset_seconds in
    let finish = instant finish ~year ~previous_offset:daylight.offset_seconds in
    let year_seconds =
      let days = if Date.days_in_month ~year ~month:Month.Feb = 29 then 366 else 365 in
      Int64.(of_int days * 86400L)
    in
    (* The IANA reference's proleptic rule interpretation suppresses forward
       seasons filling/exceeding the year (including collapsed UTC boundaries).
       If the whole Gregorian cycle emits none, the zone is perpetual daylight.
       Keep this independent of whether the requested window includes a leap
       year: skipped seasons may span several consecutive common years. *)
    if Int64.(finish < start || (start < finish && finish - start < year_seconds))
    then
      [ { at = start; year; start = true; local_type = daylight }
      ; { at = finish; year; start = false; local_type = standard }
      ]
    else []
;;

type t =
  { initial : Local_type.t
  ; changes : Transition.t array
  }

let compile parsed =
  match parsed with
  | Fixed initial -> { initial; changes = [||] }
  | Seasonal { daylight; standard = _; start = _; finish = _ } ->
    let changes =
      List.range 2000 2400
      |> List.concat_map ~f:(annual_events parsed)
      |> List.map ~f:(fun event ->
        let at = phase event.at in
        let shift = Int64.((event.at - epoch - at) / cycle) |> Int64.to_int_exn in
        { event with at; year = event.year - (shift * 400) })
      |> ordered
      |> List.map ~f:(fun { at; local_type; year = _; start = _ } ->
        { Transition.at; local_type })
      |> Array.of_list
    in
    let initial =
      if Array.is_empty changes then daylight else (Array.last changes).local_type
    in
    { initial; changes }
;;

let of_string text ~version = Result.map (parse text ~version) ~f:(Option.map ~f:compile)

let local_type t ~at =
  let at = phase at in
  let rec search lower upper =
    if lower >= upper
    then lower - 1
    else (
      let middle = lower + ((upper - lower) / 2) in
      if Int64.(t.changes.(middle).at <= at)
      then search (middle + 1) upper
      else search lower middle)
  in
  let index = search 0 (Array.length t.changes) in
  if index < 0 then t.initial else t.changes.(index).local_type
;;

let floor_cycle seconds =
  let value = Int64.(seconds / cycle) in
  if Int64.(rem seconds cycle < 0L) then Int64.(value - 1L) else value
;;

let transitions t ~first_year ~last_year =
  if
    first_year < 1
    || last_year > 9998
    || last_year < first_year
    || last_year - first_year >= 500
  then Error Error.Projection_limit
  else (
    let january year =
      Int64.(
        of_int (Date.diff (Date.create_exn ~y:year ~m:Month.Jan ~d:1) Date.unix_epoch)
        * 86400L)
    in
    let from = january first_year in
    let until = january (last_year + 1) in
    let first = floor_cycle Int64.(from - epoch) |> Int64.to_int_exn in
    let last = floor_cycle Int64.(until - 1L - epoch) |> Int64.to_int_exn in
    let changes =
      List.range first (last + 1)
      |> List.concat_map ~f:(fun index ->
        let shift = Int64.(epoch + (of_int index * cycle)) in
        Array.to_list t.changes
        |> List.filter_map ~f:(fun (change : Transition.t) ->
          let at = Int64.(change.at + shift) in
          if Int64.(at >= from && at < until) then Some { change with at } else None))
    in
    Ok changes)
;;
