open Core

module Fields = struct
  type t =
    { session : Session.t
    ; lifecycle : Session_lifecycle.Observation.t option [@sexp.option]
    ; canonical_history : Public_history.Window.t
    ; archived_revisions : int64 list [@sexp.list]
    ; effective_history : Public_history.Window.t option
    ; deferred_entries : Public_history.t list
    ; permissions : Permission.t list
    ; grants : Grant.t list
    ; jobs : Job.t list
    ; extension_status : Extension_status.t list [@sexp.list]
    ; schedules : Schedule.t list
    ; active_tool_calls : Activity.Tool.summary list
    ; active_agent_calls : Activity.Tool.summary list
    ; halted : bool
    ; halt_reason : string option
    ; failure : Protocol_error.t option
    ; revision : int64
    ; latest_event_sequence : int64
    }
  [@@deriving sexp_of]
end

type t = Fields.t [@@deriving sexp_of]

open Fields

let fields t = t

let optional_field name value encode =
  Option.map value ~f:(fun value -> name, encode value)
;;

let list encode values = `Array (List.map values ~f:encode)
let int64_to_json value = `Number (Int64.to_string value)
let nonnegative_int64 = Json_codec.bounded_int64 ~min:Int64.zero ~max:Int64.max_value

let to_json t =
  let fields =
    [ Some ("session", Session.to_json t.session)
    ; optional_field "lifecycle" t.lifecycle Session_lifecycle.Observation.to_json
    ; Some ("canonical_history", Public_history.Window.to_json t.canonical_history)
    ; Some ("archived_revisions", list int64_to_json t.archived_revisions)
    ; optional_field "effective_history" t.effective_history Public_history.Window.to_json
    ; Some ("deferred_entries", list Public_history.to_json t.deferred_entries)
    ; Some ("permissions", list Permission.to_json t.permissions)
    ; Some ("grants", list Grant.to_json t.grants)
    ; Some ("jobs", list Job.to_json t.jobs)
    ; Some ("extension_status", list Extension_status.to_json t.extension_status)
    ; Some ("schedules", list Schedule.to_json t.schedules)
    ; Some ("active_tool_calls", list Activity.Tool.summary_to_json t.active_tool_calls)
    ; Some ("active_agent_calls", list Activity.Tool.summary_to_json t.active_agent_calls)
    ; Some ("halted", if t.halted then `True else `False)
    ; optional_field "halt_reason" t.halt_reason (fun value -> `String value)
    ; optional_field "failure" t.failure Protocol_error.to_json
    ; Some ("revision", int64_to_json t.revision)
    ; Some ("latest_event_sequence", int64_to_json t.latest_event_sequence)
    ]
    |> List.filter_opt
  in
  `Object fields
;;

let decode_history fields =
  let open Result.Let_syntax in
  let%bind canonical_history =
    Json_codec.required_as fields "canonical_history" Public_history.Window.of_json
  in
  let%bind effective_history =
    Json_codec.optional_as fields "effective_history" Public_history.Window.of_json
  in
  let%map deferred_entries =
    Json_codec.required_as
      fields
      "deferred_entries"
      (Json_codec.list Public_history.of_json)
  in
  canonical_history, effective_history, deferred_entries
;;

let decode_durable_children fields =
  let open Result.Let_syntax in
  let%bind permissions =
    Json_codec.required_as fields "permissions" (Json_codec.list Permission.of_json)
  in
  let%bind grants =
    Json_codec.required_as fields "grants" (Json_codec.list Grant.of_json)
  in
  let%bind jobs = Json_codec.required_as fields "jobs" (Json_codec.list Job.of_json) in
  let%map schedules =
    Json_codec.required_as fields "schedules" (Json_codec.list Schedule.of_json)
  in
  permissions, grants, jobs, schedules
;;

let decode_active fields =
  let open Result.Let_syntax in
  let%bind active_tool_calls =
    Json_codec.required_as
      fields
      "active_tool_calls"
      (Json_codec.list Activity.Tool.summary_of_json)
  in
  let%map active_agent_calls =
    Json_codec.required_as
      fields
      "active_agent_calls"
      (Json_codec.list Activity.Tool.summary_of_json)
  in
  active_tool_calls, active_agent_calls
;;

let decode_terminal fields =
  let open Result.Let_syntax in
  let%bind halted = Json_codec.required_as fields "halted" Json_codec.bool in
  let%bind halt_reason = Json_codec.optional_as fields "halt_reason" Json_codec.string in
  let%map failure = Json_codec.optional_as fields "failure" Protocol_error.of_json in
  halted, halt_reason, failure
;;

module Activity_key = struct
  module T = struct
    type t = Activity.Key.t [@@deriving compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

let validate_activity (t : Fields.t) =
  let open Result.Let_syntax in
  let unique values =
    match
      Map.of_alist
        (module Activity_key)
        (List.map values ~f:(fun (summary : Activity.Tool.summary) ->
           summary.key, summary))
    with
    | `Duplicate_key _ ->
      Error (Protocol_error.invalid_request "duplicate snapshot activity key")
    | `Ok values -> Ok values
  in
  let%bind calls = unique t.active_tool_calls in
  let%bind agents = unique t.active_agent_calls in
  let classified (summary : Activity.Tool.summary) =
    Option.exists summary.descriptor ~f:(fun d -> Option.is_some d.classification)
  in
  if (not (Map.is_empty calls)) && Option.is_none t.session.active_operation
  then
    Error
      (Protocol_error.invalid_request "snapshot activity requires an active operation")
  else if
    Map.exists calls ~f:(fun summary ->
      match summary.Activity.Tool.state with
      | Running -> false
      | Finished _ -> true)
  then Error (Protocol_error.invalid_request "active snapshot call is already finished")
  else if
    Map.length agents <> Map.count calls ~f:classified
    || Map.existsi agents ~f:(fun ~key ~data ->
      (not (classified data))
      ||
      match Map.find calls key with
      | None -> true
      | Some original ->
        not
          (Jsonaf.exactly_equal
             (Activity.Tool.summary_to_json original)
             (Activity.Tool.summary_to_json data)))
  then
    Error
      (Protocol_error.invalid_request
         "agent activity must be the exact classified tool subset")
  else Ok ()
;;

let validate_fields t =
  let open Result.Let_syntax in
  let same_session id = Id.Session.equal t.session.id id in
  let bounded generation = generation >= 0 && generation <= t.session.generation in
  if
    Option.exists t.lifecycle ~f:(fun observation ->
      not (Session_lifecycle.Observation.matches_session observation t.session))
  then Error (Protocol_error.invalid_request "snapshot lifecycle anchor disagrees")
  else if
    Int64.(t.revision < zero || t.latest_event_sequence < zero)
    || (not (Int64.equal t.revision t.session.revision))
    || not (Int64.equal t.latest_event_sequence t.session.latest_event_sequence)
  then Error (Protocol_error.invalid_request "snapshot and session positions disagree")
  else if
    List.exists t.archived_revisions ~f:(fun revision ->
      Int64.(revision < zero || revision > t.revision))
  then Error (Protocol_error.invalid_request "invalid archived revision")
  else if
    List.exists t.extension_status ~f:(fun status ->
      not (bounded status.Extension_status.generation))
  then Error (Protocol_error.invalid_request "invalid extension generation")
  else if
    List.exists t.permissions ~f:(fun (p : Permission.t) ->
      not (same_session p.session_id && bounded p.generation))
    || List.exists t.grants ~f:(fun (g : Grant.t) -> not (same_session g.session_id))
    || List.exists t.jobs ~f:(fun (j : Job.t) ->
      not (same_session j.session_id && bounded j.generation))
    || List.exists t.schedules ~f:(fun (s : Schedule.t) ->
      not (same_session s.session_id && bounded s.generation))
  then Error (Protocol_error.invalid_request "snapshot child ownership mismatch")
  else (
    let%bind () = Projection_codec.validate (to_json t) in
    let%bind () = validate_activity t in
    let%bind () =
      Public_history.validate_unique_ids (t.canonical_history.entries @ t.deferred_entries)
    in
    let%bind () =
      match t.effective_history with
      | None -> Ok ()
      | Some window -> Public_history.Window.validate window
    in
    let%bind (_ : Session.t) = Session.of_json (Session.to_json t.session) in
    let%bind (_ : Extension_status.t list) =
      Extension_status.list_of_json
        (`Array (List.map t.extension_status ~f:Extension_status.to_json))
    in
    let validate values encode decode =
      Result.all_unit
        (List.map values ~f:(fun value ->
           Result.map (decode (encode value)) ~f:(fun _ -> ())))
    in
    let%bind () = validate t.permissions Permission.to_json Permission.of_json in
    let%bind () = validate t.grants Grant.to_json Grant.of_json in
    let%bind () = validate t.jobs Job.to_json Job.of_json in
    validate t.schedules Schedule.to_json Schedule.of_json)
;;

let create t = Result.map (validate_fields t) ~f:(fun () -> t)

let of_json json =
  let open Result.Let_syntax in
  let%bind () = Projection_codec.validate json in
  let%bind fields = Json_codec.fields json in
  let%bind session = Json_codec.required_as fields "session" Session.of_json in
  let%bind lifecycle =
    Json_codec.optional_as fields "lifecycle" Session_lifecycle.Observation.of_json
  in
  let%bind archived_revisions =
    Json_codec.optional_as fields "archived_revisions" (Json_codec.list nonnegative_int64)
  in
  let%bind extension_status =
    Json_codec.optional_as fields "extension_status" Extension_status.list_of_json
  in
  let%bind canonical_history, effective_history, deferred_entries =
    decode_history fields
  in
  let%bind permissions, grants, jobs, schedules = decode_durable_children fields in
  let%bind active_tool_calls, active_agent_calls = decode_active fields in
  let%bind halted, halt_reason, failure = decode_terminal fields in
  let%bind revision = Json_codec.required_as fields "revision" nonnegative_int64 in
  let%bind latest_event_sequence =
    Json_codec.required_as fields "latest_event_sequence" nonnegative_int64
  in
  let extension_status = Option.value extension_status ~default:[] in
  if
    List.exists extension_status ~f:(fun status ->
      status.Extension_status.generation > session.generation)
  then
    Error (Protocol_error.invalid_request "snapshot contains future extension generation")
  else if
    (not (Int64.equal revision session.revision))
    || not (Int64.equal latest_event_sequence session.latest_event_sequence)
  then Error (Protocol_error.invalid_request "snapshot and session positions disagree")
  else
    create
      { session
      ; lifecycle
      ; canonical_history
      ; archived_revisions = Option.value archived_revisions ~default:[]
      ; effective_history
      ; deferred_entries
      ; permissions
      ; grants
      ; jobs
      ; extension_status
      ; schedules
      ; active_tool_calls
      ; active_agent_calls
      ; halted
      ; halt_reason
      ; failure
      ; revision
      ; latest_event_sequence
      }
;;
