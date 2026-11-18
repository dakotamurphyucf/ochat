open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module R = Agent_store.Delegation_store
module O = Agent_store.Delegation_goal_origin

let invalid message = Error (P.Error.invalid_request message)

let domain result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let limits =
  D.Limits.create ~max_bytes:262144 ~max_depth:72 ~max_fields:16384 ~max_nodes:32768
  |> domain
  |> Result.map_error ~f:(fun error -> error.P.Error.message)
  |> Result.ok_or_failwith
;;

let digest raw = Digestif.SHA256.(to_hex (digest_string (Jsonaf.to_string raw)))
let object_ = X.object_
let required = X.required
let session_shape = X.fields_shape [ "server_id"; "session_id" ]
let source_shape = Run_record_shapes.source

let origin_shape =
  D.Shape.nullable
    (X.tagged_shape_exn
       ~discriminator:"type"
       [ "unbound", X.fields_shape [ "schema_version"; "type" ]
       ; "run", X.fields_shape [ "schema_version"; "type"; "run_id" ]
       ])
;;

module Key = struct
  type t = string [@@deriving compare, equal, sexp_of]

  let to_string t = t

  let of_json raw =
    let%bind.Result value = P.Json_codec.string raw in
    if
      String.length value = 64
      && String.for_all value ~f:(function
        | '0' .. '9' | 'a' .. 'f' -> true
        | _ -> false)
    then Ok value
    else invalid "invalid policy cause key"
  ;;

  let to_json t = `String t
end

module Goal_target = struct
  type view =
    | Local of
        { goal_id : P.Id.Goal.t
        ; episode : P.Goal_episode.t
        }
    | Inherited of O.Scope.t
  [@@deriving equal, sexp_of]

  type t = view [@@deriving equal, sexp_of]

  let view t = t
  let local ~goal_id ~episode = Ok (Local { goal_id; episode })
  let inherited scope = Inherited scope

  let owner = function
    | Local { episode; _ } -> P.Goal_episode.session episode
    | Inherited scope -> O.Scope.owner scope
  ;;

  let goal_id = function
    | Local { goal_id; _ } -> goal_id
    | Inherited scope -> O.Scope.goal_id scope
  ;;

  let source = function
    | Local { episode; _ } -> (P.Goal_episode.admission episode).source
    | Inherited scope -> O.Scope.source scope
  ;;

  let root t =
    digest
      (`Object
          [ "owner", P.Session_ref.to_json (owner t)
          ; "goal", P.Id.Goal.to_json (goal_id t)
          ])
  ;;

  let to_json = function
    | Local { goal_id; episode } ->
      `Object
        [ "kind", `String "local"
        ; "goal_id", P.Id.Goal.to_json goal_id
        ; "episode", P.Goal_episode.to_json episode
        ]
    | Inherited scope ->
      `Object [ "kind", `String "inherited"; "scope", O.Scope.to_json scope ]
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind fields = object_ raw in
    let%bind kind = required fields "kind" P.Json_codec.string in
    match kind with
    | "local" ->
      let%bind goal_id = required fields "goal_id" P.Id.Goal.of_json in
      let%bind episode = required fields "episode" P.Goal_episode.of_json in
      local ~goal_id ~episode
    | "inherited" ->
      let%map scope = required fields "scope" O.Scope.of_json in
      inherited scope
    | _ -> invalid "unknown policy target"
  ;;

  let shape =
    X.tagged_shape_exn
      ~discriminator:"kind"
      [ ( "local"
        , X.shape_exn
            [ "kind", D.Shape.value
            ; "goal_id", D.Shape.value
            ; ( "episode"
              , X.shape_exn
                  [ "session", session_shape; "admission", Run_record_shapes.receipt ] )
            ] )
      ; "inherited", X.shape_exn [ "kind", D.Shape.value; "scope", O.Scope.shape ]
      ]
  ;;
end

module Member = struct
  type kind =
    | Event
    | Invocation
    | Model_job
    | Async_job
  [@@deriving compare, equal, sexp_of]

  type t =
    { kind : kind
    ; id : string
    ; generation : int
    }
  [@@deriving compare, equal, sexp_of]

  let create kind id generation =
    if generation < 0
    then invalid "negative policy work generation"
    else Ok { kind; id; generation }
  ;;

  let event id ~generation =
    create Event (P.Id.Moderator_execution.to_string id) generation
  ;;

  let invocation id ~generation =
    create Invocation (P.Id.Invocation.to_string id) generation
  ;;

  let model_job id ~generation = create Model_job (P.Id.Job.to_string id) generation
  let async_job id ~generation = create Async_job (P.Id.Job.to_string id) generation
  let generation t = t.generation
  let kind t = t.kind
  let id t = t.id

  let kind_name = function
    | Event -> "event"
    | Invocation -> "invocation"
    | Model_job -> "model_job"
    | Async_job -> "async_job"
  ;;

  let key t = kind_name t.kind ^ ":" ^ Int.to_string t.generation ^ ":" ^ t.id

  let to_json t =
    `Object
      [ "id", `String (key t)
      ; "kind", `String (kind_name t.kind)
      ; "work_id", `String t.id
      ; "generation", X.host_counter_to_json t.generation
      ]
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind fields = object_ raw in
    let%bind generation = required fields "generation" X.host_counter_of_json in
    let%bind kind = required fields "kind" P.Json_codec.string in
    let%bind t =
      match kind with
      | "event" ->
        let%bind id = required fields "work_id" P.Id.Moderator_execution.of_json in
        event id ~generation
      | "invocation" ->
        let%bind id = required fields "work_id" P.Id.Invocation.of_json in
        invocation id ~generation
      | "model_job" ->
        let%bind id = required fields "work_id" P.Id.Job.of_json in
        model_job id ~generation
      | "async_job" ->
        let%bind id = required fields "work_id" P.Id.Job.of_json in
        async_job id ~generation
      | _ -> invalid "unknown policy work kind"
    in
    let%bind id = required fields "id" P.Json_codec.string in
    if String.equal id (key t) then Ok t else invalid "policy work identity differs"
  ;;

  let shape = X.fields_shape [ "id"; "kind"; "work_id"; "generation" ]
end

module Allocation_work = struct
  type t =
    | Event of P.Id.Moderator_execution.t
    | Invocation of P.Id.Invocation.t
    | Model_job_attempt of
        { id : P.Id.Job.t
        ; attempt : int
        }
    | Async_job_attempt of
        { id : P.Id.Job.t
        ; attempt : int
        }
  [@@deriving equal, sexp_of]

  let member t ~generation =
    match t with
    | Event id -> Member.event id ~generation
    | Invocation id -> Member.invocation id ~generation
    | Model_job_attempt { id; _ } -> Member.model_job id ~generation
    | Async_job_attempt { id; _ } -> Member.async_job id ~generation
  ;;

  let to_json = function
    | Event id ->
      `Object [ "kind", `String "event"; "id", P.Id.Moderator_execution.to_json id ]
    | Invocation id ->
      `Object [ "kind", `String "invocation"; "id", P.Id.Invocation.to_json id ]
    | Model_job_attempt { id; attempt } ->
      `Object
        [ "kind", `String "model_job_attempt"
        ; "id", P.Id.Job.to_json id
        ; "attempt", X.host_counter_to_json attempt
        ]
    | Async_job_attempt { id; attempt } ->
      `Object
        [ "kind", `String "async_job_attempt"
        ; "id", P.Id.Job.to_json id
        ; "attempt", X.host_counter_to_json attempt
        ]
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind fields = object_ raw in
    let%bind kind = required fields "kind" P.Json_codec.string in
    match kind with
    | "event" ->
      let%map id = required fields "id" P.Id.Moderator_execution.of_json in
      Event id
    | "invocation" ->
      let%map id = required fields "id" P.Id.Invocation.of_json in
      Invocation id
    | "model_job_attempt" ->
      let%bind id = required fields "id" P.Id.Job.of_json in
      let%bind attempt = required fields "attempt" X.host_counter_of_json in
      if attempt > 0
      then Ok (Model_job_attempt { id; attempt })
      else invalid "nonpositive policy Job attempt"
    | "async_job_attempt" ->
      let%bind id = required fields "id" P.Id.Job.of_json in
      let%bind attempt = required fields "attempt" X.host_counter_of_json in
      if attempt > 0
      then Ok (Async_job_attempt { id; attempt })
      else invalid "nonpositive policy Job attempt"
    | _ -> invalid "unknown policy allocation work"
  ;;

  let shape =
    X.tagged_shape_exn
      ~discriminator:"kind"
      [ "event", X.fields_shape [ "kind"; "id" ]
      ; "invocation", X.fields_shape [ "kind"; "id" ]
      ; "model_job_attempt", X.fields_shape [ "kind"; "id"; "attempt" ]
      ; "async_job_attempt", X.fields_shape [ "kind"; "id"; "attempt" ]
      ]
  ;;
end

module Coverage = struct
  type t =
    | Covered_local
    | Covered_physical
    | Causal_overlay
  [@@deriving equal, sexp_of]

  let to_json = function
    | Covered_local -> `String "covered_local"
    | Covered_physical -> `String "covered_physical"
    | Causal_overlay -> `String "causal_overlay"
  ;;

  let of_json raw =
    let%bind.Result name = P.Json_codec.string raw in
    match name with
    | "covered_local" -> Ok Covered_local
    | "covered_physical" -> Ok Covered_physical
    | "causal_overlay" -> Ok Causal_overlay
    | _ -> invalid "unknown policy coverage"
  ;;
end

let key_for ~parent_event ~callee ~generation =
  digest
    (`Object
        [ "event", P.Id.Moderator_execution.to_json parent_event
        ; "callee", P.Session_ref.to_json callee
        ; "generation", X.host_counter_to_json generation
        ])
;;

let validate_path ~caller ~callee ~(callee_source : P.Run_source.t) ~path =
  let open Result.Let_syntax in
  let%bind () =
    List.fold_result path ~init:() ~f:(fun () reference ->
      R.validate_reference reference
      |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error)
  in
  let%bind () =
    if P.Id.Server.equal (P.Session_ref.server_id caller) (P.Session_ref.server_id callee)
    then Ok ()
    else invalid "policy route crosses servers"
  in
  let rec route child seen = function
    | [] -> invalid "policy route must include a lower reference"
    | reference :: rest ->
      let parent = reference.R.Reference.key.parent_session_id in
      if
        (not (P.Id.Session.equal reference.child_session_id child))
        || List.mem seen parent ~equal:P.Id.Session.equal
      then invalid "policy route is discontinuous or cyclic"
      else (
        match rest with
        | [] ->
          if
            P.Id.Session.equal parent (P.Session_ref.session_id callee)
            && Int.equal reference.key.parent_generation callee_source.generation
          then Ok ()
          else invalid "policy route callee endpoint differs"
        | _ :: _ -> route parent (parent :: seen) rest)
  in
  route (P.Session_ref.session_id caller) [ P.Session_ref.session_id caller ] path
;;
