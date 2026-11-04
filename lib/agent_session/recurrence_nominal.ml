open! Core
module P = Agent_protocol
module R = Recurrence_rules
module I = Recurrence_identity
module F = Recurrence_definition
module D = Document_schema
module X = Persistence_codec

module Calendar_label = struct
  type t =
    { date : Date.t
    ; ofday : Time_ns.Ofday.t
    ; fold : int
    ; shifted_gap : bool
    ; zone : string
    ; provenance : R.Zone.Provenance.t
    }
  [@@deriving equal, sexp_of]
end

type label =
  | Interval
  | Calendar of Calendar_label.t
[@@deriving equal, sexp_of]

type t =
  { key : I.Occurrence.t
  ; definition_sha256 : string
  ; at : Time_ns.Alternate_sexp.t
  ; label : label
  }
[@@deriving equal, sexp_of]

let key t = t.key
let definition_sha256 t = t.definition_sha256
let at t = t.at
let label t = t.label
let invalid message = Error (P.Error.invalid_request message)

let timing result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (R.Timing_error.sexp_of_t error)))
;;

let valid_digest value =
  String.length value = 64
  && String.for_all value ~f:(function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false)
;;

let validate t ~definition =
  let open Result.Let_syntax in
  if
    not
      (I.Definition.equal (I.Occurrence.definition t.key) (F.key definition)
       && String.equal t.definition_sha256 (F.digest definition))
  then invalid "nominal occurrence differs from its captured definition"
  else (
    match F.schedule definition, t.label with
    | Interval { rule; first_due }, Interval ->
      let slot = I.Occurrence.slot t.key in
      (match R.Rule.Interval.mode rule with
       | Fixed_delay when Int64.(slot > 0L) ->
         (* A later delay has no definition-only UTC phase. Its enclosing
            frontier binds the current timestamp to exact completion evidence;
            retained historical nominal data is not a rearm proof. *)
         Ok ()
       | Fixed_rate | Fixed_delay ->
         let%bind expected =
           R.Cursor.Interval.at_slot (R.Cursor.Interval.create ~first_due) ~rule ~slot
           |> timing
         in
         if Time_ns.equal expected t.at
         then Ok ()
         else invalid "interval nominal time differs from its immutable phase")
    | Calendar rule, Calendar label ->
      if
        R.Rule.Calendar.matches rule label.date
        && Time_ns.Ofday.equal (R.Rule.Calendar.ofday rule) label.ofday
        && String.equal (R.Rule.Calendar.zone rule) label.zone
        && ((not label.shifted_gap)
            || R.Rule.Calendar.equal_gap (R.Rule.Calendar.gap rule) At_gap_end)
        && ((not (R.Rule.Calendar.equal_repeat (R.Rule.Calendar.repeat rule) Earlier))
            || Int.equal label.fold 0)
      then Ok ()
      else invalid "calendar nominal label differs from its captured rule"
    | Interval _, Calendar _ | Calendar _, Interval ->
      invalid "nominal occurrence label differs from its schedule kind")
;;

let create_interval definition ~slot =
  let open Result.Let_syntax in
  let%bind key = I.Occurrence.create ~definition:(F.key definition) ~slot in
  match F.schedule definition with
  | Calendar _ -> invalid "interval nominal requires interval schedule"
  | Interval { rule; first_due } ->
    let%bind at =
      R.Cursor.Interval.at_slot (R.Cursor.Interval.create ~first_due) ~rule ~slot
      |> timing
    in
    Ok { key; definition_sha256 = F.digest definition; at; label = Interval }
;;

let of_interval_cursor definition cursor =
  let open Result.Let_syntax in
  let%bind key =
    I.Occurrence.create
      ~definition:(F.key definition)
      ~slot:(R.Cursor.Interval.slot cursor)
  in
  let t =
    { key
    ; definition_sha256 = F.digest definition
    ; at = R.Cursor.Interval.due_at cursor
    ; label = Interval
    }
  in
  let%map () = validate t ~definition in
  t
;;

let create_calendar definition ~slot ~(occurrence : R.Cursor.Calendar.Occurrence.t) =
  let open Result.Let_syntax in
  let%bind key = I.Occurrence.create ~definition:(F.key definition) ~slot in
  let t =
    { key
    ; definition_sha256 = F.digest definition
    ; at = occurrence.at
    ; label =
        Calendar
          { date = occurrence.date
          ; ofday = occurrence.ofday
          ; fold = occurrence.fold
          ; shifted_gap = occurrence.shifted_gap
          ; zone = occurrence.zone
          ; provenance = occurrence.provenance
          }
    }
  in
  let%map () = validate t ~definition in
  t
;;

let provenance_json p =
  `Object
    [ "source", `String (R.Zone.Provenance.source p)
    ; "sha256", `String (R.Zone.Provenance.sha256 p)
    ]
;;

let label_json = function
  | Interval -> `Object [ "kind", `String "interval" ]
  | Calendar l ->
    `Object
      [ "kind", `String "calendar"
      ; "date", `String (Date.to_string l.date)
      ; "ofday", `String (Time_ns.Ofday.to_string l.ofday)
      ; "fold", X.integer_json l.fold
      ; "shifted_gap", X.bool_json l.shifted_gap
      ; "zone", `String l.zone
      ; "provenance", provenance_json l.provenance
      ]
;;

let to_json t =
  `Object
    [ "key", I.Occurrence.to_json t.key
    ; "definition_sha256", `String t.definition_sha256
    ; "at", P.Timestamp.to_json (P.Timestamp.of_time_ns t.at)
    ; "label", label_json t.label
    ]
;;

let limits =
  D.Limits.create ~max_bytes:16384 ~max_depth:24 ~max_fields:128 ~max_nodes:256
  |> Result.map_error ~f:(fun e -> Sexp.to_string_hum (D.Error.sexp_of_t e))
  |> Result.ok_or_failwith
;;

let date_json = function
  | `String text when String.length text = 10 ->
    (match Or_error.try_with (fun () -> Date.of_string text) with
     | Ok value when String.equal (Date.to_string value) text -> Ok value
     | Ok _ | Error _ -> invalid "invalid nominal calendar date")
  | _ -> invalid "invalid nominal calendar date"
;;

let ofday_json = function
  | `String text when String.length text > 0 && String.length text <= 18 ->
    (match Or_error.try_with (fun () -> Time_ns.Ofday.of_string text) with
     | Ok value when Time_ns.Ofday.compare value Time_ns.Ofday.start_of_next_day < 0 ->
       Ok value
     | Ok _ | Error _ -> invalid "invalid nominal calendar ofday")
  | _ -> invalid "invalid nominal calendar ofday"
;;

let label_of_json json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind kind = X.required fields "kind" P.Json_codec.string in
  match kind with
  | "interval" -> Ok Interval
  | "calendar" ->
    let%bind date = X.required fields "date" date_json in
    let%bind ofday = X.required fields "ofday" ofday_json in
    let%bind fold = X.required fields "fold" X.integer in
    let%bind shifted_gap = X.required fields "shifted_gap" P.Json_codec.bool in
    let%bind zone = X.required fields "zone" P.Json_codec.string in
    let%bind () = R.Zone.validate_name zone |> timing in
    let%bind provenance =
      X.required fields "provenance" (fun json ->
        let%bind fields = X.object_ json in
        let%bind source = X.required fields "source" P.Json_codec.string in
        let%bind sha256 = X.required fields "sha256" P.Json_codec.string in
        R.Zone.Provenance.create ~source ~sha256 |> timing)
    in
    if fold < 0 || fold > 4096 || (shifted_gap && not (Int.equal fold 0))
    then invalid "invalid nominal calendar fold"
    else Ok (Calendar { date; ofday; fold; shifted_gap; zone; provenance })
  | _ -> invalid "unsupported nominal label variant"
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () =
    D.Json.validate ~limits json
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
  in
  let%bind fields = X.object_ json in
  let%bind key = X.required fields "key" I.Occurrence.of_json in
  let%bind definition_sha256 =
    X.required fields "definition_sha256" P.Json_codec.string
  in
  let%bind at = X.required fields "at" P.Timestamp.of_json in
  let%bind label = X.required fields "label" label_of_json in
  if not (valid_digest definition_sha256)
  then invalid "invalid nominal definition digest"
  else Ok { key; definition_sha256; at = P.Timestamp.to_time_ns at; label }
;;

let shape =
  let v = D.Shape.value in
  X.shape_exn
    [ "key", I.Occurrence.shape
    ; "definition_sha256", v
    ; "at", v
    ; ( "label"
      , X.tagged_shape_exn
          ~discriminator:"kind"
          [ "interval", X.fields_shape [ "kind" ]
          ; ( "calendar"
            , X.shape_exn
                [ "kind", v
                ; "date", v
                ; "ofday", v
                ; "fold", v
                ; "shifted_gap", v
                ; "zone", v
                ; "provenance", X.fields_shape [ "source"; "sha256" ]
                ] )
          ] )
    ]
;;
