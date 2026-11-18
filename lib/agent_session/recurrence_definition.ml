open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module C = D.Extension_carrier
module I = Recurrence_identity
module Schedule = P.Recurrence_schedule

module Goal_binding = struct
  module Capture = struct
    type t =
      { owner : P.Session_ref.t
      ; goal_id : P.Id.Goal.t
      ; creator_episode : P.Goal_episode.t
      ; goal_revision_at_capture : int64
      }
    [@@deriving equal]

    let owner t = t.owner
    let goal_id t = t.goal_id
    let creator_episode t = t.creator_episode
    let goal_revision_at_capture t = t.goal_revision_at_capture

    let create ~owner ~goal_id ~creator_episode ~goal_revision_at_capture =
      let open Result.Let_syntax in
      let%bind _ = P.Id.Goal.of_json (P.Id.Goal.to_json goal_id) in
      if
        P.Session_ref.equal owner (P.Goal_episode.session creator_episode)
        && Int64.(goal_revision_at_capture >= 0L)
      then Ok { owner; goal_id; creator_episode; goal_revision_at_capture }
      else
        Error
          (P.Error.invalid_request
             "recurrence Goal capture differs from its owner or revision")
    ;;

    let to_json t =
      `Object
        [ "owner", P.Session_ref.to_json t.owner
        ; "goal_id", P.Id.Goal.to_json t.goal_id
        ; "creator_episode", P.Goal_episode.to_json t.creator_episode
        ; "goal_revision_at_capture", X.int64_json t.goal_revision_at_capture
        ]
    ;;

    let of_json json =
      let open Result.Let_syntax in
      let%bind fields = X.object_ json in
      let%bind owner = X.required fields "owner" P.Session_ref.of_json in
      let%bind goal_id = X.required fields "goal_id" P.Id.Goal.of_json in
      let%bind creator_episode =
        X.required fields "creator_episode" P.Goal_episode.of_json
      in
      let%bind goal_revision_at_capture =
        X.required fields "goal_revision_at_capture" (function
          | `String text when String.length text <= 19 ->
            X.nonnegative_int64 (`String text)
          | _ ->
            Error (P.Error.invalid_request "invalid recurrence Goal capture revision"))
      in
      create ~owner ~goal_id ~creator_episode ~goal_revision_at_capture
    ;;
  end

  type t =
    | Unbound
    | Controller_goal of Capture.t
  [@@deriving equal]

  let semantics = P.Session_storage_semantics.recurrence_controller_goal
  let field = "goal_binding"

  let decode json =
    match D.Json.field json ~name:field with
    | Absent -> Ok Unbound
    | Null -> Error (P.Error.invalid_request "recurrence Goal binding cannot be null")
    | Value json ->
      Result.map (Capture.of_json json) ~f:(fun value -> Controller_goal value)
  ;;

  let check_document document =
    let present =
      match D.Json.field (D.Document.payload document) ~name:field with
      | Absent -> false
      | Null | Value _ -> true
    in
    let marked =
      List.mem (D.Document.required_semantics document) semantics ~equal:String.equal
    in
    match present, marked with
    | true, false -> Error (D.Error.Required_semantics_unknown ("missing " ^ semantics))
    | false, true ->
      Agent_store.Document_fields.invalid
        field
        "required recurrence Goal binding is absent"
    | true, true | false, false -> Ok ()
  ;;
end

type projection =
  { key : I.Definition.t
  ; creator : P.Id.Principal.t
  ; source : P.Run_source.t
  ; birth_lifecycle_revision : P.Session_lifecycle.Revision.t
  ; schedule : Schedule.t
  ; target : Recurrence_target.t
  ; policy : Recurrence_policy.t
  ; created_at : P.Timestamp.t
  ; goal_binding : Goal_binding.t
  }

type t =
  { projection : projection
  ; document : D.Document.t
  ; sha256 : string
  }

let key t = t.projection.key
let creator t = t.projection.creator
let source t = t.projection.source
let birth_lifecycle_revision t = t.projection.birth_lifecycle_revision
let schedule t = t.projection.schedule
let target t = t.projection.target
let policy t = t.projection.policy
let created_at t = t.projection.created_at
let goal_binding t = t.projection.goal_binding
let digest t = t.sha256
let to_document t = t.document

let initial_cursor t =
  match schedule t with
  | Interval { first_due; _ } -> Recurrence_cursor.create_interval (key t) ~first_due
  | Calendar rule -> Recurrence_cursor.create_calendar (key t) ~rule
;;

let limits =
  D.Limits.create
    ~max_bytes:(26 * 1024 * 1024)
    ~max_depth:160
    ~max_fields:32768
    ~max_nodes:65536
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let protocol result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let validate (t : projection) =
  let open Result.Let_syntax in
  let%bind _ = P.Id.Principal.of_json (P.Id.Principal.to_json t.creator) in
  let%bind () = P.Run_source.validate t.source in
  let%bind _ =
    P.Session_lifecycle.Revision.of_int64
      (P.Session_lifecycle.Revision.to_int64 t.birth_lifecycle_revision)
  in
  let%bind _ = P.Timestamp.of_json (P.Timestamp.to_json t.created_at) in
  let%bind _ = Schedule.of_json (Schedule.to_json t.schedule) in
  let%bind _ = Recurrence_policy.of_json (Recurrence_policy.to_json t.policy) in
  let%bind _ = Recurrence_target.to_document t.target |> protocol in
  match t.goal_binding, Recurrence_target.view t.target with
  | Unbound, _ -> Ok ()
  | Controller_goal _, Existing_session _ ->
    Error (P.Error.invalid_request "recurrence Goal binding requires a fresh child")
  | Controller_goal capture, Fresh_child _ ->
    let owner = Goal_binding.Capture.owner capture in
    let episode = Goal_binding.Capture.creator_episode capture in
    if
      P.Session_ref.equal owner (I.Definition.controller t.key)
      && P.Run_source.equal (P.Goal_episode.admission episode).source t.source
    then Ok ()
    else
      Error
        (P.Error.invalid_request
           "recurrence Goal binding differs from the definition owner or source")
;;

let encode (t : projection) =
  let%map.Result target = Recurrence_target.to_document t.target |> protocol in
  let base =
    [ "key", I.Definition.to_json t.key
    ; "creator", P.Id.Principal.to_json t.creator
    ; "source", P.Run_source.to_json t.source
    ; ( "birth_lifecycle_revision"
      , X.int64_json (P.Session_lifecycle.Revision.to_int64 t.birth_lifecycle_revision) )
    ; "schedule", Schedule.to_json t.schedule
    ; "target", D.Document.json target
    ; "policy", Recurrence_policy.to_json t.policy
    ; "created_at", P.Timestamp.to_json t.created_at
    ]
  in
  match t.goal_binding with
  | Unbound -> `Object base
  | Controller_goal capture ->
    `Object (base @ [ Goal_binding.field, Goal_binding.Capture.to_json capture ])
;;

let decode json =
  let open Result.Let_syntax in
  let%bind f = X.object_ json in
  let%bind key = X.required f "key" I.Definition.of_json in
  let%bind creator = X.required f "creator" P.Id.Principal.of_json in
  let%bind source = X.required f "source" P.Run_source.of_json in
  let%bind birth_lifecycle_revision =
    X.required f "birth_lifecycle_revision" (fun json ->
      let%bind n =
        match json with
        | `String text when String.length text <= 19 -> X.nonnegative_int64 (`String text)
        | _ -> Error (P.Error.invalid_request "invalid recurrence birth revision")
      in
      P.Session_lifecycle.Revision.of_int64 n)
  in
  let%bind schedule = X.required f "schedule" Schedule.of_json in
  let%bind target =
    X.required f "target" (fun json ->
      let%bind document =
        D.Document.inspect ~limits:Recurrence_target.limits json |> protocol
      in
      Recurrence_target.of_document document |> protocol)
  in
  let%bind policy = X.required f "policy" Recurrence_policy.of_json in
  let%bind created_at = X.required f "created_at" P.Timestamp.of_json in
  let%bind goal_binding = Goal_binding.decode json in
  let t =
    { key
    ; creator
    ; source
    ; birth_lifecycle_revision
    ; schedule
    ; target
    ; policy
    ; created_at
    ; goal_binding
    }
  in
  let%map () = validate t in
  t
;;

let shape =
  let v = D.Shape.value in
  X.shape_exn
    [ "key", I.Definition.shape
    ; "creator", v
    ; "source", Run_record_shapes.source
    ; "birth_lifecycle_revision", v
    ; "schedule", Schedule.shape (* Target owns its complete nested universal carrier. *)
    ; "target", v
    ; "policy", Recurrence_policy.shape
    ; "created_at", v
    ; Goal_binding.field, v
    ]
;;

let codec =
  D.Domain_codec.create_validated
    ~limits
    ~kind:"ochat.recurrence_definition"
    ~version:1
    ~shape
    ~supported_semantics:[ Goal_binding.semantics ]
    ~validate:(fun t -> X.document_result (validate t))
    ~decode:(fun json -> X.document_result (decode json))
    ~encode:(fun t -> X.document_result (encode t))
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let admitted projection document =
  let%map.Result canonical =
    P.Json_codec.canonical_string (D.Document.json document) |> X.document_result
  in
  { projection; document; sha256 = Digestif.SHA256.(digest_string canonical |> to_hex) }
;;

let of_document document =
  let open Result.Let_syntax in
  let%bind () = Goal_binding.check_document document in
  let%bind carrier = D.Domain_codec.decode codec document in
  admitted (C.value carrier) document
;;

let create_with_binding
      ~goal_binding
      ~key
      ~creator
      ~source
      ~birth_lifecycle_revision
      ~schedule
      ~target
      ~policy
      ~created_at
  =
  let open Result.Let_syntax in
  let projection =
    { key
    ; creator
    ; source
    ; birth_lifecycle_revision
    ; schedule
    ; target
    ; policy
    ; created_at
    ; goal_binding
    }
  in
  let%bind document =
    D.Domain_codec.encode codec (C.of_authored_value projection) |> protocol
  in
  let%bind document =
    match goal_binding with
    | Unbound -> Ok document
    | Controller_goal _ ->
      (match D.Document.json document with
       | `Object fields ->
         let fields = List.Assoc.remove fields ~equal:String.equal "required_semantics" in
         D.Document.inspect
           ~limits
           (`Object
               (fields
                @ [ "required_semantics", `Array [ `String Goal_binding.semantics ] ]))
         |> protocol
       | _ -> assert false)
  in
  admitted projection document |> protocol
;;

let create = create_with_binding ~goal_binding:Unbound
let create_bound ~binding = create_with_binding ~goal_binding:(Controller_goal binding)
