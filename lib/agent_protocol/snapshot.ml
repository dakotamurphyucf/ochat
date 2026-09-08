open Core

type t =
  { session : Session.t
  ; canonical_history : History.Window.t
  ; archived_revisions : int64 list [@sexp.list]
  ; effective_history : History.Window.t option
  ; deferred_entries : History.entry list
  ; permissions : Permission.t list
  ; grants : Grant.t list
  ; jobs : Job.t list
  ; extension_status : Extension_status.t list [@sexp.list]
  ; schedules : Schedule.t list
  ; active_tool_calls : Jsonaf.t list
  ; active_agent_calls : Jsonaf.t list
  ; halted : bool
  ; halt_reason : string option
  ; failure : Protocol_error.t option
  ; revision : int64
  ; latest_event_sequence : int64
  }
[@@deriving sexp]

let optional_field name value encode =
  Option.map value ~f:(fun value -> name, encode value)
;;

let list encode values = `Array (List.map values ~f:encode)
let int64_to_json value = `Number (Int64.to_string value)
let nonnegative_int64 = Json_codec.bounded_int64 ~min:Int64.zero ~max:Int64.max_value

let to_json t =
  let fields =
    [ Some ("session", Session.to_json t.session)
    ; Some ("canonical_history", History.Window.to_json t.canonical_history)
    ; Some ("archived_revisions", list int64_to_json t.archived_revisions)
    ; optional_field "effective_history" t.effective_history History.Window.to_json
    ; Some ("deferred_entries", list History.entry_to_json t.deferred_entries)
    ; Some ("permissions", list Permission.to_json t.permissions)
    ; Some ("grants", list Grant.to_json t.grants)
    ; Some ("jobs", list Job.to_json t.jobs)
    ; Some ("extension_status", list Extension_status.to_json t.extension_status)
    ; Some ("schedules", list Schedule.to_json t.schedules)
    ; Some ("active_tool_calls", `Array t.active_tool_calls)
    ; Some ("active_agent_calls", `Array t.active_agent_calls)
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
    Json_codec.required_as fields "canonical_history" History.Window.of_json
  in
  let%bind effective_history =
    Json_codec.optional_as fields "effective_history" History.Window.of_json
  in
  let%map deferred_entries =
    Json_codec.required_as
      fields
      "deferred_entries"
      (Json_codec.list History.entry_of_json)
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
    Json_codec.required_as fields "active_tool_calls" (Json_codec.list (fun x -> Ok x))
  in
  let%map active_agent_calls =
    Json_codec.required_as fields "active_agent_calls" (Json_codec.list (fun x -> Ok x))
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

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind session = Json_codec.required_as fields "session" Session.of_json in
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
    Ok
      { session
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
