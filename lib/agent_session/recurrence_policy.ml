open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec

type misfire =
  | Skip
  | Coalesce
  | Catch_up of int
[@@deriving equal, sexp_of]

type t =
  { misfire : misfire
  ; grace_ms : int64
  ; max_pending : int
  ; max_history : int
  ; max_calendar_steps : int
  }
[@@deriving equal, sexp_of]

let invalid message = Error (P.Error.invalid_request message)

let create ~misfire ~grace_ms ~max_pending ~max_history ~max_calendar_steps =
  let catch_up =
    match misfire with
    | Skip | Coalesce -> true
    | Catch_up limit -> limit >= 1 && limit <= 32
  in
  if
    (not catch_up)
    || Int64.(grace_ms < 0L || grace_ms > max_value / 1_000_000L)
    || max_pending < 1
    || max_pending > 32
    || max_history < 1
    || max_history > 256
    || max_calendar_steps < 1
    || max_calendar_steps > 4096
  then invalid "recurrence policy exceeds supported finite bounds"
  else Ok { misfire; grace_ms; max_pending; max_history; max_calendar_steps }
;;

let default =
  create
    ~misfire:Coalesce
    ~grace_ms:60_000L
    ~max_pending:1
    ~max_history:64
    ~max_calendar_steps:256
  |> Result.map_error ~f:(fun error -> error.P.Error.message)
  |> Result.ok_or_failwith
;;

let misfire t = t.misfire
let grace_ms t = t.grace_ms
let grace_ns t = Int64.(t.grace_ms * 1_000_000L)
let max_pending t = t.max_pending
let max_history t = t.max_history
let max_calendar_steps t = t.max_calendar_steps

let to_json t =
  let misfire =
    match t.misfire with
    | Skip -> `Object [ "kind", `String "skip" ]
    | Coalesce -> `Object [ "kind", `String "coalesce" ]
    | Catch_up limit ->
      `Object [ "kind", `String "catch_up"; "limit", X.integer_json limit ]
  in
  `Object
    [ "misfire", misfire
    ; "grace_ms", X.int64_json t.grace_ms
    ; "max_pending", X.integer_json t.max_pending
    ; "max_history", X.integer_json t.max_history
    ; "max_calendar_steps", X.integer_json t.max_calendar_steps
    ]
;;

let limits =
  D.Limits.create ~max_bytes:8192 ~max_depth:8 ~max_fields:64 ~max_nodes:128
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () =
    D.Json.validate ~limits json
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
  in
  let%bind fields = X.object_ json in
  let%bind misfire = X.required fields "misfire" X.object_ in
  let%bind kind = X.required misfire "kind" P.Json_codec.string in
  let%bind misfire =
    match kind with
    | "skip" -> Ok Skip
    | "coalesce" -> Ok Coalesce
    | "catch_up" ->
      let%map limit =
        X.required misfire "limit" (P.Json_codec.bounded_int ~min:1 ~max:32)
      in
      Catch_up limit
    | _ -> invalid "unsupported recurrence misfire policy"
  in
  let%bind grace_ms =
    X.required fields "grace_ms" (function
      | `String text when String.length text <= 19 -> X.nonnegative_int64 (`String text)
      | _ -> invalid "invalid recurrence grace milliseconds")
  in
  let%bind max_pending =
    X.required fields "max_pending" (P.Json_codec.bounded_int ~min:1 ~max:32)
  in
  let%bind max_history =
    X.required fields "max_history" (P.Json_codec.bounded_int ~min:1 ~max:256)
  in
  let%bind max_calendar_steps =
    X.required fields "max_calendar_steps" (P.Json_codec.bounded_int ~min:1 ~max:4096)
  in
  create ~misfire ~grace_ms ~max_pending ~max_history ~max_calendar_steps
;;

let shape =
  let v = D.Shape.value in
  X.shape_exn
    [ ( "misfire"
      , X.tagged_shape_exn
          ~discriminator:"kind"
          [ "skip", X.fields_shape [ "kind" ]
          ; "coalesce", X.fields_shape [ "kind" ]
          ; "catch_up", X.fields_shape [ "kind"; "limit" ]
          ] )
    ; "grace_ms", v
    ; "max_pending", v
    ; "max_history", v
    ; "max_calendar_steps", v
    ]
;;
