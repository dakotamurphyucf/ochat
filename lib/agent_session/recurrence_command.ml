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

module Goal_owner_request = struct
  type t =
    { goal_id : P.Id.Goal.t
    ; expected_goal_revision : int64
    }

  let create ~goal_id ~expected_goal_revision =
    let open Result.Let_syntax in
    let%bind _ = P.Id.Goal.of_json (P.Id.Goal.to_json goal_id) in
    if Int64.(expected_goal_revision >= 0L)
    then Ok { goal_id; expected_goal_revision }
    else Error (P.Error.invalid_request "invalid expected recurrence Goal revision")
  ;;

  let to_json t =
    `Object
      [ "goal_id", P.Id.Goal.to_json t.goal_id
      ; "expected_goal_revision", X.int64_json t.expected_goal_revision
      ]
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
  ; goal_owner : Goal_owner_request.t option
  ; shared_intent_digest : string option
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
      ; goal_owner = None
      ; shared_intent_digest = None
      }
;;

let with_goal_owner t goal_owner =
  if Option.is_some t.shared_intent_digest
  then Error (P.Error.invalid_request "shared recurrence intent is immutable")
  else (
    match t.mutation with
    | Create (_, capture) | Edit (_, capture) ->
      (match Recurrence_target.view capture.target with
       | Fresh_child _ -> Ok { t with goal_owner = Some goal_owner }
       | Existing_session _ ->
         Error
           (P.Error.invalid_request "recurrence Goal ownership requires a fresh child"))
    | Enable _ | Pause _ | Retire _ ->
      Error
        (P.Error.invalid_request
           "recurrence Goal selection requires a definition capture"))
;;

let of_shared_intent ~session (intent : P.Recurrence_control.Intent.t) ~target =
  let module W = P.Recurrence_control in
  let open Result.Let_syntax in
  let capture (capture : W.Capture.t) =
    let%bind target =
      Result.of_option
        target
        ~error:(P.Error.invalid_request "missing recurrence capture")
    in
    let matches =
      match capture.selector, Recurrence_target.view target with
      | Existing selected, Existing_session actual ->
        P.Session_ref.equal selected.session actual.session
        && Document_schema.Json.equal
             (P.Session.Message_content.to_json selected.input)
             (P.Session.Message_content.to_json actual.input)
      | ( Authored_child selected
        , Fresh_child { source_kind = Authored_tool authored; mode; _ } ) ->
        String.equal selected.name authored.name
        && Option.equal P.Run.Mode.equal (Some selected.mode) mode
      | _ -> false
    in
    if matches
    then Ok { Capture.schedule = capture.schedule; target; policy = capture.policy }
    else
      Error (P.Error.invalid_request "captured recurrence target differs from selector")
  in
  let%bind mutation, basis, owner =
    match intent.action with
    | Create a ->
      let%map captured = capture a.capture in
      Mutation.Create (a.id, captured), Some a.basis, a.capture.goal_owner
    | Edit a ->
      let%map captured = capture a.capture in
      Mutation.Edit (a.id, captured), Some a.basis, a.capture.goal_owner
    | (Enable _ | Pause _ | Retire _) when Option.is_some target ->
      Error (P.Error.invalid_request "unexpected recurrence target")
    | Enable a -> Ok (Mutation.Enable a.id, Some a.basis, None)
    | Pause a -> Ok (Mutation.Pause a.id, None, None)
    | Retire a -> Ok (Mutation.Retire a.id, None, None)
  in
  let%bind command =
    create
      ~session_id:(P.Session_ref.session_id session)
      ~generation:intent.generation
      ~source:(Option.map basis ~f:(fun b -> b.source))
      ~expected_lifecycle_revision:
        (Option.map basis ~f:(fun b -> b.expected_lifecycle_revision))
      ~expected_catalog_revision:intent.expected_catalog_revision
      ~expected_control_revision:(W.Action.expected_control_revision intent.action)
      ~key:intent.key
      ~mutation
  in
  let%bind command =
    match owner with
    | None -> Ok command
    | Some owner ->
      let%bind owner =
        Goal_owner_request.create
          ~goal_id:owner.goal_id
          ~expected_goal_revision:owner.expected_goal_revision
      in
      with_goal_owner command owner
  in
  let%map digest = W.Intent.digest intent ~session in
  { command with shared_intent_digest = Some digest }
;;

let digest_fields t ~goal_fields =
  let open Result.Let_syntax in
  let%bind mutation = Mutation.to_json t.mutation in
  let fields =
    [ "kind", `String "recurrence_command_v1"
    ; "session_id", P.Id.Session.to_json t.session_id
    ; "generation", X.host_counter_to_json t.generation
    ; "source", X.option_json P.Run_source.to_json t.source
    ; ( "expected_lifecycle_revision"
      , X.option_json
          (fun revision -> X.int64_json (P.Session_lifecycle.Revision.to_int64 revision))
          t.expected_lifecycle_revision )
    ; "expected_catalog_revision", X.int64_json t.expected_catalog_revision
    ; "expected_control_revision", X.option_json X.int64_json t.expected_control_revision
    ; "key", P.Idempotency_key.to_json t.key
    ; "mutation", mutation
    ]
  in
  let%map encoded = P.Json_codec.canonical_string (`Object (fields @ goal_fields)) in
  Digestif.SHA256.(digest_string encoded |> to_hex)
;;

let digest t =
  let goal_fields =
    Option.to_list
      (Option.map t.goal_owner ~f:(fun goal ->
         "goal_owner", Goal_owner_request.to_json goal))
  in
  match t.shared_intent_digest with
  | Some original -> Ok original
  | None -> digest_fields t ~goal_fields
;;

let digest_native t ~(binding : Recurrence_definition.Goal_binding.t) =
  if Option.is_some t.shared_intent_digest
  then Error (P.Error.invalid_request "shared recurrence intent is not native input")
  else (
    match t.goal_owner, binding with
    | Some _, _ ->
      Error (P.Error.invalid_request "an operator Goal selection is not native input")
    | None, Unbound -> digest_fields t ~goal_fields:[]
    | None, Controller_goal binding ->
      let frozen = Recurrence_definition.Goal_binding.Capture.to_json binding in
      digest_fields t ~goal_fields:[ "native_goal_binding", frozen ])
;;
