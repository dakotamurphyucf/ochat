open! Core
module P = Agent_protocol
module X = Persistence_codec

module Capture = struct
  type t =
    { schedule : Recurrence_definition.Schedule.t
    ; target : Recurrence_target.t
    ; policy : Recurrence_policy.t
    }

  let validate t =
    let open Result.Let_syntax in
    let%bind _ =
      Recurrence_definition.Schedule.of_json
        (Recurrence_definition.Schedule.to_json t.schedule)
    in
    let%map _ = Recurrence_policy.of_json (Recurrence_policy.to_json t.policy) in
    ()
  ;;

  let to_json t =
    let%map.Result digest = Recurrence_target.digest t.target in
    `Object
      [ "schedule", Recurrence_definition.Schedule.to_json t.schedule
      ; "target_sha256", `String digest
      ; "policy", Recurrence_policy.to_json t.policy
      ]
  ;;
end

module Mutation = struct
  type t =
    | Create of P.Id.Recurrence.t * Capture.t
    | Edit of P.Id.Recurrence.t * Capture.t
    | Enable of P.Id.Recurrence.t
    | Pause of P.Id.Recurrence.t
    | Retire of P.Id.Recurrence.t

  let id = function
    | Create (id, _) | Edit (id, _) | Enable id | Pause id | Retire id -> id
  ;;

  let needs_source = function
    | Create _ | Edit _ | Enable _ -> true
    | Pause _ | Retire _ -> false
  ;;

  let to_json t =
    let%map.Result kind, fields =
      match t with
      | Create (_, capture) ->
        Result.map (Capture.to_json capture) ~f:(fun json ->
          "create", [ "capture", json ])
      | Edit (_, capture) ->
        Result.map (Capture.to_json capture) ~f:(fun json -> "edit", [ "capture", json ])
      | Enable _ -> Ok ("enable", [])
      | Pause _ -> Ok ("pause", [])
      | Retire _ -> Ok ("retire", [])
    in
    `Object
      ([ "kind", `String kind; "recurrence_id", P.Id.Recurrence.to_json (id t) ] @ fields)
  ;;
end

type t =
  { session_id : P.Id.Session.t
  ; generation : int
  ; source : P.Run_source.t option
  ; expected_lifecycle_revision : P.Session_lifecycle.Revision.t option
  ; expected_catalog_revision : int64
  ; expected_control_revision : int64 option
  ; key : P.Idempotency_key.t
  ; mutation : Mutation.t
  }

let create
      ~session_id
      ~generation
      ~source
      ~expected_lifecycle_revision
      ~expected_catalog_revision
      ~expected_control_revision
      ~key
      ~mutation
  =
  let open Result.Let_syntax in
  let%bind _ = P.Id.Session.of_json (P.Id.Session.to_json session_id) in
  let%bind _ = P.Id.Recurrence.of_json (P.Id.Recurrence.to_json (Mutation.id mutation)) in
  let%bind _ = P.Idempotency_key.of_json (P.Idempotency_key.to_json key) in
  let%bind () =
    match source with
    | None -> Ok ()
    | Some source -> P.Run_source.validate source
  in
  let%bind () =
    match expected_lifecycle_revision with
    | None -> Ok ()
    | Some revision ->
      Result.map
        (P.Session_lifecycle.Revision.of_int64
           (P.Session_lifecycle.Revision.to_int64 revision))
        ~f:(fun _ -> ())
  in
  let%bind () =
    match mutation with
    | Create (_, capture) | Edit (_, capture) -> Capture.validate capture
    | Enable _ | Pause _ | Retire _ -> Ok ()
  in
  let frontier =
    match mutation, expected_control_revision with
    | Create _, None -> true
    | (Edit _ | Enable _ | Pause _ | Retire _), Some value -> Int64.(value >= 0L)
    | Create _, Some _ | (Edit _ | Enable _ | Pause _ | Retire _), None -> false
  in
  if
    generation < 0
    || Int64.(expected_catalog_revision < 0L)
    || (not frontier)
    || Bool.(Mutation.needs_source mutation <> Option.is_some source)
    || Bool.(Mutation.needs_source mutation <> Option.is_some expected_lifecycle_revision)
    || Option.exists source ~f:(fun source ->
      source.P.Run_source.generation <> generation)
  then Error (P.Error.invalid_request "invalid recurrence command expectations")
  else
    Ok
      { session_id
      ; generation
      ; source
      ; expected_lifecycle_revision
      ; expected_catalog_revision
      ; expected_control_revision
      ; key
      ; mutation
      }
;;

let digest t =
  let open Result.Let_syntax in
  let%bind mutation = Mutation.to_json t.mutation in
  let json =
    `Object
      [ "kind", `String "recurrence_command_v1"
      ; "session_id", P.Id.Session.to_json t.session_id
      ; "generation", X.host_counter_to_json t.generation
      ; "source", X.option_json P.Run_source.to_json t.source
      ; ( "expected_lifecycle_revision"
        , X.option_json
            (fun revision ->
               X.int64_json (P.Session_lifecycle.Revision.to_int64 revision))
            t.expected_lifecycle_revision )
      ; "expected_catalog_revision", X.int64_json t.expected_catalog_revision
      ; ( "expected_control_revision"
        , X.option_json X.int64_json t.expected_control_revision )
      ; "key", P.Idempotency_key.to_json t.key
      ; "mutation", mutation
      ]
  in
  let%map encoded = P.Json_codec.canonical_string json in
  Digestif.SHA256.(digest_string encoded |> to_hex)
;;
