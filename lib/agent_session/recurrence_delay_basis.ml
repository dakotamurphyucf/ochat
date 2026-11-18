open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module R = Recurrence_rules
module I = Recurrence_identity
module F = Recurrence_definition
module Execution = Recurrence_execution
module Intent = Recurrence_execution_intent
module Origin = Recurrence_run_origin
module Reference = Recurrence_run_reference

type projection =
  { definition_sha256 : string
  ; nominal_at : Time_ns.Alternate_sexp.t
  ; origin : Origin.t
  ; terminal : Reference.t
  }

type t =
  { projection : projection
  ; json : Jsonaf.t
  }

let definition_sha256 t = t.projection.definition_sha256
let nominal_at t = t.projection.nominal_at
let origin t = t.projection.origin
let terminal t = t.projection.terminal
let occurrence t = Origin.occurrence (origin t)
let completed_at t = (Reference.receipt (terminal t)).committed_at
let to_json t = t.json
let equal a b = D.Json.equal a.json b.json
let invalid message = Error (P.Error.invalid_request message)

let protocol result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let kind = "ochat.recurrence_delay_basis"

(* Measure the exact complete wrapper with maximum-width known scalar values.
   Replacing the two nulls with complete child profiles adds their whole quotas,
   not merely their payloads. A finite extension allowance is charged in every
   enclosing future-basis reservation, including before its first publication. *)
let wrapper =
  D.Document.create
    ~limits:D.Limits.default
    ~kind
    ~version:1
    ~payload:
      (`Object
          [ "definition_sha256", `String (String.make 64 'f')
          ; ( "nominal_at"
            , P.Timestamp.to_json (P.Timestamp.of_time_ns Time_ns.max_value_representable)
            )
          ; "origin", `Null
          ; "terminal", `Null
          ])
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
  |> D.Document.json
;;

let wrapper_growth =
  Capacity_growth.measure wrapper ~limits:D.Limits.default
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (P.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let limits =
  D.Limits.create
    ~max_bytes:
      (Capacity_growth.encoded_bytes wrapper_growth
       - 8
       + D.Limits.max_bytes Origin.limits
       + D.Limits.max_bytes Reference.limits
       + 4096)
    ~max_depth:
      (2
       + Int.max (D.Limits.max_depth Origin.limits) (D.Limits.max_depth Reference.limits)
      )
    ~max_fields:
      (Capacity_growth.fields wrapper_growth
       + D.Limits.max_fields Origin.limits
       + D.Limits.max_fields Reference.limits
       + 64)
    ~max_nodes:
      (Capacity_growth.nodes wrapper_growth
       - 2
       + D.Limits.max_nodes Origin.limits
       + D.Limits.max_nodes Reference.limits
       + 128)
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let validate_projection p =
  let open Result.Let_syntax in
  let%bind () = Reference.validate_origin p.terminal ~origin:p.origin in
  let definitive =
    match Reference.outcome p.terminal with
    | Completed | Failed _ | Cancelled | Limited -> true
    | Interrupted -> false
  in
  if
    definitive
    && String.length p.definition_sha256 = 64
    && String.for_all p.definition_sha256 ~f:(function
      | '0' .. '9' | 'a' .. 'f' -> true
      | _ -> false)
  then Ok ()
  else invalid "fixed-delay basis lacks a definitive terminal association"
;;

let encode p =
  `Object
    [ "definition_sha256", `String p.definition_sha256
    ; "nominal_at", P.Timestamp.to_json (P.Timestamp.of_time_ns p.nominal_at)
    ; "origin", Origin.to_json p.origin
    ; "terminal", Reference.to_json p.terminal
    ]
;;

let decode json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind definition_sha256 =
    X.required fields "definition_sha256" P.Json_codec.string
  in
  let%bind nominal_at =
    X.required fields "nominal_at" P.Timestamp.of_json
    |> Result.map ~f:P.Timestamp.to_time_ns
  in
  let%bind origin = X.required fields "origin" Origin.of_json in
  let%bind terminal = X.required fields "terminal" Reference.of_json in
  let p = { definition_sha256; nominal_at; origin; terminal } in
  let%map () = validate_projection p in
  p
;;

let codec =
  X.codec_exn
    ~version:1
    ~limits
    ~kind
    ~shape:(X.fields_shape [ "definition_sha256"; "nominal_at"; "origin"; "terminal" ])
    ~decode
    ~encode:(fun p -> Ok (encode p))
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind document = D.Document.inspect ~limits json |> protocol in
  let%map carrier = D.Domain_codec.decode codec document |> protocol in
  { projection = D.Extension_carrier.value carrier; json }
;;

let validate t ~definition =
  let open Result.Let_syntax in
  let%bind () = validate_projection t.projection in
  match F.schedule definition with
  | Interval { rule; first_due } ->
    if
      R.Rule.Interval.equal_mode (R.Rule.Interval.mode rule) Fixed_delay
      && I.Definition.equal (I.Occurrence.definition (occurrence t)) (F.key definition)
      && String.equal (definition_sha256 t) (F.digest definition)
      && ((not (Int64.equal (I.Occurrence.slot (occurrence t)) 0L))
          || Time_ns.equal (nominal_at t) first_due)
    then Ok ()
    else invalid "fixed-delay basis differs from its exact definition"
  | Calendar _ -> invalid "calendar definition has no fixed-delay basis"
;;

let next_interval t ~definition =
  let open Result.Let_syntax in
  let%bind () = validate t ~definition in
  match F.schedule definition with
  | Calendar _ -> assert false
  | Interval { rule; _ } ->
    let timing result =
      Result.map_error result ~f:(fun error ->
        P.Error.invalid_request (Sexp.to_string_hum (R.Timing_error.sexp_of_t error)))
    in
    let%bind previous =
      R.Cursor.Interval.of_json
        (`Object
            [ "version", `Number "1"
            ; "slot", `Number (Int64.to_string (I.Occurrence.slot (occurrence t)))
            ; "due_at", P.Timestamp.to_json (P.Timestamp.of_time_ns (nominal_at t))
            ])
      |> timing
    in
    R.Cursor.Interval.after_completion
      previous
      ~rule
      ~completed_at:(P.Timestamp.to_time_ns (completed_at t))
    |> timing
;;

let create execution ~definition =
  let open Result.Let_syntax in
  let intent = Execution.intent execution in
  let attempt = Execution.current_attempt execution in
  match Execution.Attempt.origin attempt, Execution.Attempt.terminal attempt with
  | Some origin, Some terminal ->
    let%bind () =
      Origin.validate origin ~intent ~attempt:(Execution.Attempt.request attempt)
    in
    let projection =
      { definition_sha256 = Recurrence_nominal.definition_sha256 (Intent.nominal intent)
      ; nominal_at = Recurrence_nominal.at (Intent.nominal intent)
      ; origin
      ; terminal
      }
    in
    let%bind () = validate_projection projection in
    let%bind document =
      D.Domain_codec.encode codec (D.Extension_carrier.of_authored_value projection)
      |> protocol
    in
    let%bind t = of_json (D.Document.json document) in
    let%map () = validate t ~definition in
    t
  | None, _ | Some _, None ->
    invalid "fixed-delay basis requires an exact settled execution"
;;
