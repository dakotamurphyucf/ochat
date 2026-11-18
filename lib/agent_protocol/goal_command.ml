open! Core
module G = Goal
module J = Json_codec
module W = Goal_wire
module Error = Protocol_error

module Mutation = struct
  type t =
    | Create of
        { id : Id.Goal.t
        ; objective : G.Objective.t
        ; budget : G.Budget.t
        }
    | Revise_objective of Id.Goal.t * G.Objective.t
    | Record_progress of Id.Goal.t * G.Progress.t
    | Hold of Id.Goal.t * G.Hold.t
    | Change_budget of Id.Goal.t * G.Budget.t
    | Adopt_source of Id.Goal.t
    | Abandon of Id.Goal.t * G.Reason.t
  [@@deriving sexp_of]

  let goal_id = function
    | Create { id; _ } -> id
    | Revise_objective (id, _)
    | Record_progress (id, _)
    | Hold (id, _)
    | Change_budget (id, _)
    | Adopt_source id
    | Abandon (id, _) -> id
  ;;

  let needs_source = function
    | Hold _ | Abandon _ -> false
    | Create _ | Revise_objective _ | Record_progress _ | Change_budget _ | Adopt_source _
      -> true
  ;;

  let to_json mutation =
    let kind, extra =
      match mutation with
      | Create { objective; budget; _ } ->
        ( "create"
        , [ "objective", `String (G.Objective.to_string objective)
          ; "budget", G.Budget.to_json budget
          ] )
      | Revise_objective (_, objective) ->
        "objective", [ "objective", `String (G.Objective.to_string objective) ]
      | Record_progress (_, progress) ->
        "progress", [ "progress", `String (G.Progress.to_string progress) ]
      | Hold (_, reason) -> "hold", [ "reason", Goal.Hold.to_json reason ]
      | Change_budget (_, budget) -> "budget", [ "budget", G.Budget.to_json budget ]
      | Adopt_source _ -> "adopt_source", []
      | Abandon (_, reason) ->
        "abandon", [ "reason", `String (G.Reason.to_string reason) ]
    in
    `Object
      ([ "kind", `String kind; "goal_id", Id.Goal.to_json (goal_id mutation) ] @ extra)
  ;;
end

type t =
  { session_id : Id.Session.t
  ; generation : int
  ; source : Run_source.t option
  ; expected_catalog_revision : int64
  ; expected_goal_revision : int64 option
  ; key : Idempotency_key.t
  ; mutation : Mutation.t
  }

let create
      ~session_id
      ~generation
      ~source
      ~expected_catalog_revision
      ~expected_goal_revision
      ~key
      ~mutation
  =
  let open Result.Let_syntax in
  let%bind () =
    match source with
    | None -> Ok ()
    | Some source -> Run_source.validate source
  in
  let%bind _ = Id.Session.of_json (Id.Session.to_json session_id) in
  let%bind _ = Id.Goal.of_json (Id.Goal.to_json (Mutation.goal_id mutation)) in
  let%bind _ = Idempotency_key.of_json (Idempotency_key.to_json key) in
  let valid_goal_revision =
    match mutation, expected_goal_revision with
    | Mutation.Create _, None -> true
    | Create _, Some _ -> false
    | ( ( Revise_objective _
        | Record_progress _
        | Hold _
        | Change_budget _
        | Adopt_source _
        | Abandon _ )
      , Some revision ) -> Int64.(revision >= 0L)
    | ( ( Revise_objective _
        | Record_progress _
        | Hold _
        | Change_budget _
        | Adopt_source _
        | Abandon _ )
      , None ) -> false
  in
  if
    generation < 0
    || Int64.(expected_catalog_revision < 0L)
    || (not valid_goal_revision)
    || Bool.(Mutation.needs_source mutation <> Option.is_some source)
    || Option.exists source ~f:(fun source -> source.Run_source.generation <> generation)
  then Error (Error.invalid_request "invalid goal command fences")
  else
    Ok
      { session_id
      ; generation
      ; source
      ; expected_catalog_revision
      ; expected_goal_revision
      ; key
      ; mutation
      }
;;

let to_json t =
  `Object
    [ "kind", `String "goal_command_v1"
    ; "session_id", Id.Session.to_json t.session_id
    ; "generation", W.generation_json t.generation
    ; "source", W.option_json Run_source.to_json t.source
    ; "expected_catalog_revision", W.counter_json t.expected_catalog_revision
    ; "expected_goal_revision", W.option_json W.counter_json t.expected_goal_revision
    ; "key", Idempotency_key.to_json t.key
    ; "mutation", Mutation.to_json t.mutation
    ]
;;

let digest t =
  let%map.Result encoded = J.canonical_string (to_json t) in
  Digestif.SHA256.(digest_string encoded |> to_hex)
;;

let mutation_of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind kind = J.required_as fields "kind" J.string in
  let%bind id = J.required_as fields "goal_id" Id.Goal.of_json in
  let with_fields names f =
    let%bind () = Extension_codec.closed fields ("kind" :: "goal_id" :: names) in
    f ()
  in
  match kind with
  | "create" ->
    with_fields [ "objective"; "budget" ] (fun () ->
      let%bind objective = J.required_as fields "objective" G.Objective.of_json in
      let%map budget = J.required_as fields "budget" W.budget in
      Mutation.Create { id; objective; budget })
  | "objective" ->
    with_fields [ "objective" ] (fun () ->
      let%map objective = J.required_as fields "objective" G.Objective.of_json in
      Mutation.Revise_objective (id, objective))
  | "progress" ->
    with_fields [ "progress" ] (fun () ->
      let%map progress = J.required_as fields "progress" G.Progress.of_json in
      Mutation.Record_progress (id, progress))
  | "hold" ->
    with_fields [ "reason" ] (fun () ->
      let%map reason = J.required_as fields "reason" W.hold in
      Mutation.Hold (id, reason))
  | "budget" ->
    with_fields [ "budget" ] (fun () ->
      let%map budget = J.required_as fields "budget" W.budget in
      Mutation.Change_budget (id, budget))
  | "adopt_source" -> with_fields [] (fun () -> Ok (Mutation.Adopt_source id))
  | "abandon" ->
    with_fields [ "reason" ] (fun () ->
      let%map reason = J.required_as fields "reason" G.Reason.of_json in
      Mutation.Abandon (id, reason))
  | _ -> W.invalid "unknown Goal metadata command"
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = Extension_codec.validate_json ~max_bytes:32768 ~max_depth:16 json in
  let%bind fields =
    W.fields
      json
      [ "kind"
      ; "session_id"
      ; "generation"
      ; "source"
      ; "expected_catalog_revision"
      ; "expected_goal_revision"
      ; "key"
      ; "mutation"
      ]
  in
  let%bind kind = J.required_as fields "kind" J.string in
  let%bind () =
    if String.equal kind "goal_command_v1"
    then Ok ()
    else W.invalid "unknown Goal command format"
  in
  let%bind session_id = J.required_as fields "session_id" Id.Session.of_json in
  let%bind generation = J.required_as fields "generation" W.generation in
  let%bind source = J.required_as fields "source" (W.nullable W.source) in
  let%bind expected_catalog_revision =
    J.required_as fields "expected_catalog_revision" W.counter
  in
  let%bind expected_goal_revision =
    J.required_as fields "expected_goal_revision" (W.nullable W.counter)
  in
  let%bind key = J.required_as fields "key" Idempotency_key.of_json in
  let%bind mutation = J.required_as fields "mutation" mutation_of_json in
  create
    ~session_id
    ~generation
    ~source
    ~expected_catalog_revision
    ~expected_goal_revision
    ~key
    ~mutation
;;
