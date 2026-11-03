open! Core
module P = Agent_protocol
module R = Recurrence_rules
module I = Recurrence_identity
module D = Document_schema
module X = Persistence_codec

type position =
  | Interval of R.Cursor.Interval.t
  | Calendar of
      { cursor : R.Cursor.Calendar.t
      ; next_slot : int64
      }
  | Awaiting_completion of I.Occurrence.t
  | Exhausted
[@@deriving equal, sexp_of]

type t =
  { definition : I.Definition.t
  ; position : position
  }
[@@deriving equal, sexp_of]

let definition t = t.definition
let position t = t.position

let create_interval definition ~first_due =
  { definition; position = Interval (R.Cursor.Interval.create ~first_due) }
;;

let create_calendar definition ~rule =
  { definition
  ; position = Calendar { cursor = R.Cursor.Calendar.create rule; next_slot = 0L }
  }
;;

let invalid message = Error (P.Error.invalid_request message)

let validate t ~rule =
  match t.position, rule with
  | Interval _, R.Rule.Interval _
  | Calendar _, Calendar _
  | Exhausted, (Interval _ | Calendar _) -> Ok ()
  | Awaiting_completion occurrence, Interval rule ->
    if
      R.Rule.Interval.equal_mode (R.Rule.Interval.mode rule) Fixed_delay
      && I.Definition.equal t.definition (I.Occurrence.definition occurrence)
    then Ok ()
    else invalid "recurrence completion frontier has wrong mode or definition"
  | Interval _, Calendar _ | Calendar _, Interval _ | Awaiting_completion _, Calendar _ ->
    invalid "recurrence cursor kind differs from immutable rule"
;;

let to_json t =
  let position =
    match t.position with
    | Interval cursor ->
      `Object [ "kind", `String "interval"; "cursor", R.Cursor.Interval.to_json cursor ]
    | Calendar { cursor; next_slot } ->
      `Object
        [ "kind", `String "calendar"
        ; "cursor", R.Cursor.Calendar.to_json cursor
        ; "next_slot", X.int64_json next_slot
        ]
    | Awaiting_completion occurrence ->
      `Object
        [ "kind", `String "awaiting_completion"
        ; "occurrence", I.Occurrence.to_json occurrence
        ]
    | Exhausted -> `Object [ "kind", `String "exhausted" ]
  in
  `Object [ "definition", I.Definition.to_json t.definition; "position", position ]
;;

let limits =
  D.Limits.create ~max_bytes:65536 ~max_depth:32 ~max_fields:1024 ~max_nodes:2048
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let domain result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (R.Timing_error.sexp_of_t error)))
;;

let of_json json ~rule =
  let open Result.Let_syntax in
  let%bind () =
    D.Json.validate ~limits json
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
  in
  let%bind fields = X.object_ json in
  let%bind definition = X.required fields "definition" I.Definition.of_json in
  let%bind position = X.required fields "position" X.object_ in
  let%bind kind = X.required position "kind" P.Json_codec.string in
  let%bind position =
    match kind with
    | "interval" ->
      let%map cursor =
        X.required position "cursor" (fun json -> domain (R.Cursor.Interval.of_json json))
      in
      Interval cursor
    | "calendar" ->
      let%bind cursor =
        X.required position "cursor" (fun json -> domain (R.Cursor.Calendar.of_json json))
      in
      let%map next_slot =
        X.required position "next_slot" (function
          | `String text when String.length text <= 19 ->
            X.nonnegative_int64 (`String text)
          | _ -> invalid "invalid recurrence calendar slot")
      in
      Calendar { cursor; next_slot }
    | "awaiting_completion" ->
      let%map occurrence = X.required position "occurrence" I.Occurrence.of_json in
      Awaiting_completion occurrence
    | "exhausted" -> Ok Exhausted
    | _ -> invalid "unknown recurrence cursor kind"
  in
  let t = { definition; position } in
  let%map () = validate t ~rule in
  t
;;

let shape =
  let v = D.Shape.value in
  X.shape_exn
    [ "definition", I.Definition.shape
    ; ( "position"
      , X.tagged_shape_exn
          ~discriminator:"kind"
          [ "interval", X.shape_exn [ "kind", v; "cursor", v ]
          ; "calendar", X.shape_exn [ "kind", v; "cursor", v; "next_slot", v ]
          ; ( "awaiting_completion"
            , X.shape_exn [ "kind", v; "occurrence", I.Occurrence.shape ] )
          ; "exhausted", X.shape_exn [ "kind", v ]
          ] )
    ]
;;
