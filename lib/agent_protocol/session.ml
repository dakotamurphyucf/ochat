open Core

type execution_host =
  | Daemon
  | Embedded
[@@deriving compare, equal, sexp]

type stop_mode =
  | Graceful
  | Cancel
[@@deriving compare, equal, sexp]

type liveness =
  | Detached
  | Owner_bound of
      { disconnect_grace_ms : int
      ; stop_mode : stop_mode
      }
  | Process_bound
[@@deriving compare, equal, sexp]

type persistence =
  | Durable
  | Transient
[@@deriving compare, equal, sexp]

type desired_state =
  | Running
  | Stopped
[@@deriving compare, equal, sexp]

type observed_state =
  | Stopped
  | Queued_for_slot
  | Starting
  | Recovering
  | Idle
  | Running_turn of Id.Operation.t
  | Compacting of Id.Operation.t
  | Waiting_for_permission of Id.Permission.t
  | Stopping
  | Failed of Protocol_error.t
[@@deriving sexp]

type attachment_mode =
  | Owner_read_write
  | Read_write
  | Read_only
[@@deriving compare, equal, sexp]

let optional_field name value encode =
  Option.map value ~f:(fun value -> name, encode value)
;;

let nonnegative_int64 = Json_codec.bounded_int64 ~min:Int64.zero ~max:Int64.max_value
let int64_to_json value = `Number (Int64.to_string value)

let host_to_string = function
  | Daemon -> "daemon"
  | Embedded -> "embedded"
;;

let host_of_json =
  Json_codec.enum ~name:"execution host" [ "daemon", Daemon; "embedded", Embedded ]
;;

let stop_mode_to_string = function
  | Graceful -> "graceful"
  | Cancel -> "cancel"
;;

let stop_mode_of_json =
  Json_codec.enum ~name:"stop mode" [ "graceful", Graceful; "cancel", Cancel ]
;;

let persistence_to_string = function
  | Durable -> "durable"
  | Transient -> "transient"
;;

let persistence_of_json =
  Json_codec.enum
    ~name:"persistence policy"
    [ "durable", Durable; "transient", Transient ]
;;

let desired_state_to_string = function
  | Running -> "running"
  | Stopped -> "stopped"
;;

let desired_state_of_json =
  Json_codec.enum ~name:"desired session state" [ "running", Running; "stopped", Stopped ]
;;

let attachment_mode_to_string = function
  | Owner_read_write -> "owner_read_write"
  | Read_write -> "read_write"
  | Read_only -> "read_only"
;;

let attachment_mode_of_json =
  Json_codec.enum
    ~name:"attachment mode"
    [ "owner_read_write", Owner_read_write
    ; "read_write", Read_write
    ; "read_only", Read_only
    ]
;;

let liveness_to_json = function
  | Detached -> `Object [ "type", `String "detached" ]
  | Process_bound -> `Object [ "type", `String "process_bound" ]
  | Owner_bound { disconnect_grace_ms; stop_mode } ->
    `Object
      [ "type", `String "owner_bound"
      ; "disconnect_grace_ms", `Number (Int.to_string disconnect_grace_ms)
      ; "stop_mode", `String (stop_mode_to_string stop_mode)
      ]
;;

let owner_bound_of_fields fields =
  let open Result.Let_syntax in
  let%bind disconnect_grace_ms =
    Json_codec.required_as
      fields
      "disconnect_grace_ms"
      (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%map stop_mode = Json_codec.required_as fields "stop_mode" stop_mode_of_json in
  Owner_bound { disconnect_grace_ms; stop_mode }
;;

let liveness_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind encoded = Json_codec.required_as fields "type" Json_codec.string in
  match encoded with
  | "detached" -> Ok Detached
  | "owner_bound" -> owner_bound_of_fields fields
  | "process_bound" -> Ok Process_bound
  | _ -> Error (Protocol_error.invalid_request "unknown liveness policy")
;;

let observed_state_to_json = function
  | Stopped -> `Object [ "type", `String "stopped" ]
  | Queued_for_slot -> `Object [ "type", `String "queued_for_slot" ]
  | Starting -> `Object [ "type", `String "starting" ]
  | Recovering -> `Object [ "type", `String "recovering" ]
  | Idle -> `Object [ "type", `String "idle" ]
  | Running_turn id ->
    `Object [ "type", `String "running_turn"; "operation_id", Id.Operation.to_json id ]
  | Compacting id ->
    `Object [ "type", `String "compacting"; "operation_id", Id.Operation.to_json id ]
  | Waiting_for_permission id ->
    `Object
      [ "type", `String "waiting_for_permission"
      ; "permission_id", Id.Permission.to_json id
      ]
  | Stopping -> `Object [ "type", `String "stopping" ]
  | Failed error ->
    `Object [ "type", `String "failed"; "error", Protocol_error.to_json error ]
;;

let observed_state_of_fields fields encoded =
  match encoded with
  | "stopped" -> Ok Stopped
  | "queued_for_slot" -> Ok Queued_for_slot
  | "starting" -> Ok Starting
  | "recovering" -> Ok Recovering
  | "idle" -> Ok Idle
  | "running_turn" ->
    Result.map
      (Json_codec.required_as fields "operation_id" Id.Operation.of_json)
      ~f:(fun id -> Running_turn id)
  | "compacting" ->
    Result.map
      (Json_codec.required_as fields "operation_id" Id.Operation.of_json)
      ~f:(fun id -> Compacting id)
  | "waiting_for_permission" ->
    Result.map
      (Json_codec.required_as fields "permission_id" Id.Permission.of_json)
      ~f:(fun id -> Waiting_for_permission id)
  | "stopping" -> Ok Stopping
  | "failed" ->
    Result.map
      (Json_codec.required_as fields "error" Protocol_error.of_json)
      ~f:(fun error -> Failed error)
  | _ -> Error (Protocol_error.invalid_request "unknown observed session state")
;;

let observed_state_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind encoded = Json_codec.required_as fields "type" Json_codec.string in
  observed_state_of_fields fields encoded
;;

let validate_path kind path =
  if String.is_empty path || String.mem path '\000'
  then Error (Protocol_error.invalid_request (kind ^ " path is invalid"))
  else Ok path
;;

module Prompt_ref = struct
  type t =
    | Catalog of Id.Prompt_definition.t
    | Local_path of string
    | Generated of Id.Prompt_revision.t
  [@@deriving sexp]

  let to_json = function
    | Catalog id ->
      `Object [ "type", `String "catalog"; "prompt_id", Id.Prompt_definition.to_json id ]
    | Local_path path -> `Object [ "type", `String "local_path"; "path", `String path ]
    | Generated revision ->
      `Object
        [ "type", `String "generated"
        ; "revision_id", Id.Prompt_revision.to_json revision
        ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind encoded = Json_codec.required_as fields "type" Json_codec.string in
    match encoded with
    | "catalog" ->
      Result.map
        (Json_codec.required_as fields "prompt_id" Id.Prompt_definition.of_json)
        ~f:(fun id -> Catalog id)
    | "local_path" ->
      let%bind path = Json_codec.required_as fields "path" Json_codec.string in
      Result.map (validate_path "prompt" path) ~f:(fun path -> Local_path path)
    | "generated" ->
      Result.map
        (Json_codec.required_as fields "revision_id" Id.Prompt_revision.of_json)
        ~f:(fun revision -> Generated revision)
    | _ -> Error (Protocol_error.invalid_request "unknown prompt reference")
  ;;
end

module Workspace_request = struct
  type t =
    | Configured of Id.Workspace_definition.t
    | Current
    | Local_path of string
  [@@deriving sexp]

  let to_json = function
    | Configured id ->
      `Object
        [ "type", `String "configured"
        ; "workspace_id", Id.Workspace_definition.to_json id
        ]
    | Current -> `Object [ "type", `String "current" ]
    | Local_path path -> `Object [ "type", `String "local_path"; "path", `String path ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind encoded = Json_codec.required_as fields "type" Json_codec.string in
    match encoded with
    | "configured" ->
      Result.map
        (Json_codec.required_as fields "workspace_id" Id.Workspace_definition.of_json)
        ~f:(fun id -> Configured id)
    | "current" -> Ok Current
    | "local_path" ->
      let%bind path = Json_codec.required_as fields "path" Json_codec.string in
      Result.map (validate_path "workspace" path) ~f:(fun path -> Local_path path)
    | _ -> Error (Protocol_error.invalid_request "unknown workspace request")
  ;;
end

let labels_to_json labels =
  `Object (List.map labels ~f:(fun (name, value) -> name, `String value))
;;

let labels_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  Result.all
    (List.map (Json_codec.to_alist fields) ~f:(fun (name, value) ->
       let%map value = Json_codec.string value in
       name, value))
;;

let validate_labels labels =
  let names = List.map labels ~f:fst in
  if List.exists names ~f:String.is_empty
  then Error (Protocol_error.invalid_request "session label key must be nonempty")
  else if Option.is_some (List.find_a_dup names ~compare:String.compare)
  then Error (Protocol_error.invalid_request "session label keys must be unique")
  else
    Ok (List.sort labels ~compare:(fun (left, _) (right, _) -> String.compare left right))
;;

let validate_optional_text name = function
  | Some value when String.is_empty value ->
    Error (Protocol_error.invalid_request (name ^ " must be nonempty when present"))
  | value -> Ok value
;;

let validate_policy execution_host liveness persistence =
  match execution_host, liveness, persistence with
  | Daemon, Detached, Durable -> Ok ()
  | Daemon, Owner_bound _, (Durable | Transient) -> Ok ()
  | Embedded, Process_bound, (Durable | Transient) -> Ok ()
  | Daemon, Detached, Transient ->
    Error (Protocol_error.invalid_request "detached daemon sessions must be durable")
  | Daemon, Process_bound, _ ->
    Error (Protocol_error.invalid_request "daemon sessions cannot be process-bound")
  | Embedded, (Detached | Owner_bound _), _ ->
    Error (Protocol_error.invalid_request "embedded sessions must be process-bound")
;;

module Spec = struct
  type t =
    { execution_host : execution_host
    ; prompt : Prompt_ref.t
    ; workspace : Workspace_request.t
    ; liveness : liveness
    ; persistence : persistence
    ; permission_profile : string option
    ; start_immediately : bool
    ; display_name : string option
    ; labels : (string * string) list
    }
  [@@deriving sexp]

  let create
        ~execution_host
        ~prompt
        ~workspace
        ~liveness
        ~persistence
        ?permission_profile
        ~start_immediately
        ?display_name
        ~labels
        ()
    =
    let open Result.Let_syntax in
    let%bind () = validate_policy execution_host liveness persistence in
    let%bind () =
      match prompt, persistence with
      | Prompt_ref.Generated _, Transient ->
        Error
          (Protocol_error.invalid_request
             "generated sessions require durable delegation admission")
      | _ -> Ok ()
    in
    let%bind permission_profile =
      validate_optional_text "permission profile" permission_profile
    in
    let%bind display_name = validate_optional_text "display name" display_name in
    let%map labels = validate_labels labels in
    { execution_host
    ; prompt
    ; workspace
    ; liveness
    ; persistence
    ; permission_profile
    ; start_immediately
    ; display_name
    ; labels
    }
  ;;

  let to_json t =
    let fields =
      [ Some ("execution_host", `String (host_to_string t.execution_host))
      ; Some ("prompt", Prompt_ref.to_json t.prompt)
      ; Some ("workspace", Workspace_request.to_json t.workspace)
      ; Some ("liveness", liveness_to_json t.liveness)
      ; Some ("persistence", `String (persistence_to_string t.persistence))
      ; optional_field "permission_profile" t.permission_profile (fun value ->
          `String value)
      ; Some ("start_immediately", if t.start_immediately then `True else `False)
      ; optional_field "display_name" t.display_name (fun value -> `String value)
      ; Some ("labels", labels_to_json t.labels)
      ]
      |> List.filter_opt
    in
    `Object fields
  ;;

  let decode_policy fields =
    let open Result.Let_syntax in
    let%bind execution_host =
      Json_codec.required_as fields "execution_host" host_of_json
    in
    let%bind liveness = Json_codec.required_as fields "liveness" liveness_of_json in
    let%map persistence =
      Json_codec.required_as fields "persistence" persistence_of_json
    in
    execution_host, liveness, persistence
  ;;

  let decode_metadata fields =
    let open Result.Let_syntax in
    let%bind permission_profile =
      Json_codec.optional_as fields "permission_profile" Json_codec.string
    in
    let%bind start_immediately =
      Json_codec.required_as fields "start_immediately" Json_codec.bool
    in
    let%bind display_name =
      Json_codec.optional_as fields "display_name" Json_codec.string
    in
    let%map labels = Json_codec.required_as fields "labels" labels_of_json in
    permission_profile, start_immediately, display_name, labels
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind execution_host, liveness, persistence = decode_policy fields in
    let%bind prompt = Json_codec.required_as fields "prompt" Prompt_ref.of_json in
    let%bind workspace =
      Json_codec.required_as fields "workspace" Workspace_request.of_json
    in
    let%bind permission_profile, start_immediately, display_name, labels =
      decode_metadata fields
    in
    create
      ~execution_host
      ~prompt
      ~workspace
      ~liveness
      ~persistence
      ?permission_profile
      ~start_immediately
      ?display_name
      ~labels
      ()
  ;;
end

type t =
  { id : Id.Session.t
  ; creator : Id.Principal.t option
  ; created_at : Timestamp.t
  ; updated_at : Timestamp.t
  ; generation : int
  ; spec : Spec.t
  ; desired_state : desired_state
  ; observed_state : observed_state
  ; prompt_revision : Id.Prompt_revision.t option
  ; workspace_instance : Id.Workspace_instance.t option
  ; active_operation : Operation.t option
  ; revision : int64
  ; latest_event_sequence : int64
  }
[@@deriving sexp]

let to_json t =
  let fields =
    [ Some ("id", Id.Session.to_json t.id)
    ; optional_field "creator" t.creator Id.Principal.to_json
    ; Some ("created_at", Timestamp.to_json t.created_at)
    ; Some ("updated_at", Timestamp.to_json t.updated_at)
    ; Some ("generation", `Number (Int.to_string t.generation))
    ; Some ("spec", Spec.to_json t.spec)
    ; Some ("desired_state", `String (desired_state_to_string t.desired_state))
    ; Some ("observed_state", observed_state_to_json t.observed_state)
    ; optional_field "prompt_revision" t.prompt_revision Id.Prompt_revision.to_json
    ; optional_field
        "workspace_instance"
        t.workspace_instance
        Id.Workspace_instance.to_json
    ; optional_field "active_operation" t.active_operation Operation.to_json
    ; Some ("revision", int64_to_json t.revision)
    ; Some ("latest_event_sequence", int64_to_json t.latest_event_sequence)
    ]
    |> List.filter_opt
  in
  `Object fields
;;

let decode_summary_identity fields =
  let open Result.Let_syntax in
  let%bind id = Json_codec.required_as fields "id" Id.Session.of_json in
  let%bind creator = Json_codec.optional_as fields "creator" Id.Principal.of_json in
  let%bind created_at = Json_codec.required_as fields "created_at" Timestamp.of_json in
  let%bind updated_at = Json_codec.required_as fields "updated_at" Timestamp.of_json in
  let%map generation =
    Json_codec.required_as
      fields
      "generation"
      (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  id, creator, created_at, updated_at, generation
;;

let decode_summary_state fields =
  let open Result.Let_syntax in
  let%bind spec = Json_codec.required_as fields "spec" Spec.of_json in
  let%bind desired_state =
    Json_codec.required_as fields "desired_state" desired_state_of_json
  in
  let%bind observed_state =
    Json_codec.required_as fields "observed_state" observed_state_of_json
  in
  let%map active_operation =
    Json_codec.optional_as fields "active_operation" Operation.of_json
  in
  spec, desired_state, observed_state, active_operation
;;

let decode_summary_references fields =
  let open Result.Let_syntax in
  let%bind prompt_revision =
    Json_codec.optional_as fields "prompt_revision" Id.Prompt_revision.of_json
  in
  let%map workspace_instance =
    Json_codec.optional_as fields "workspace_instance" Id.Workspace_instance.of_json
  in
  prompt_revision, workspace_instance
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind id, creator, created_at, updated_at, generation =
    decode_summary_identity fields
  in
  let%bind spec, desired_state, observed_state, active_operation =
    decode_summary_state fields
  in
  let%bind prompt_revision, workspace_instance = decode_summary_references fields in
  let%bind revision = Json_codec.required_as fields "revision" nonnegative_int64 in
  let%bind latest_event_sequence =
    Json_codec.required_as fields "latest_event_sequence" nonnegative_int64
  in
  if Timestamp.compare updated_at created_at < 0
  then Error (Protocol_error.invalid_request "session update precedes creation")
  else
    Ok
      { id
      ; creator
      ; created_at
      ; updated_at
      ; generation
      ; spec
      ; desired_state
      ; observed_state
      ; prompt_revision
      ; workspace_instance
      ; active_operation
      ; revision
      ; latest_event_sequence
      }
;;

module Owner_lease = struct
  type t =
    { generation : int64
    ; expires_at : Timestamp.t
    ; disconnect_grace_until : Timestamp.t option
    ; principal_id : Id.Principal.t option [@sexp.option]
    ; reclaim_token_sha256 : string option [@sexp.option]
    }
  [@@deriving sexp]

  let to_json t =
    let fields =
      [ Some ("generation", int64_to_json t.generation)
      ; Some ("expires_at", Timestamp.to_json t.expires_at)
      ; optional_field "disconnect_grace_until" t.disconnect_grace_until Timestamp.to_json
      ; optional_field "principal_id" t.principal_id Id.Principal.to_json
      ]
      |> List.filter_opt
    in
    `Object fields
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind generation = Json_codec.required_as fields "generation" nonnegative_int64 in
    let%bind expires_at = Json_codec.required_as fields "expires_at" Timestamp.of_json in
    let%map disconnect_grace_until =
      Json_codec.optional_as fields "disconnect_grace_until" Timestamp.of_json
    and principal_id =
      Json_codec.optional_as fields "principal_id" Id.Principal.of_json
    in
    { generation
    ; expires_at
    ; disconnect_grace_until
    ; principal_id
    ; reclaim_token_sha256 = None
    }
  ;;
end

module Attachment = struct
  type t =
    { id : Id.Attachment.t
    ; session_id : Id.Session.t
    ; mode : attachment_mode
    ; owner_lease : Owner_lease.t option
    }
  [@@deriving sexp]

  let to_json t =
    let fields =
      [ Some ("id", Id.Attachment.to_json t.id)
      ; Some ("session_id", Id.Session.to_json t.session_id)
      ; Some ("mode", `String (attachment_mode_to_string t.mode))
      ; optional_field "owner_lease" t.owner_lease Owner_lease.to_json
      ]
      |> List.filter_opt
    in
    `Object fields
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind id = Json_codec.required_as fields "id" Id.Attachment.of_json in
    let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
    let%bind mode = Json_codec.required_as fields "mode" attachment_mode_of_json in
    let%bind owner_lease =
      Json_codec.optional_as fields "owner_lease" Owner_lease.of_json
    in
    match mode, owner_lease with
    | Owner_read_write, _ -> Ok { id; session_id; mode; owner_lease }
    | (Read_write | Read_only), None -> Ok { id; session_id; mode; owner_lease }
    | (Read_write | Read_only), Some _ ->
      Error (Protocol_error.invalid_request "nonowner attachment has an owner lease")
  ;;
end

module Create_request = struct
  type t =
    { spec : Spec.t
    ; requested_mode : attachment_mode option
    ; subscribe : bool
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    let fields =
      [ Some ("spec", Spec.to_json t.spec)
      ; optional_field "requested_mode" t.requested_mode (fun mode ->
          `String (attachment_mode_to_string mode))
      ; Some ("subscribe", if t.subscribe then `True else `False)
      ; Some ("idempotency_key", Idempotency_key.to_json t.idempotency_key)
      ]
      |> List.filter_opt
    in
    `Object fields
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind spec = Json_codec.required_as fields "spec" Spec.of_json in
    let%bind requested_mode =
      Json_codec.optional_as fields "requested_mode" attachment_mode_of_json
    in
    let%bind subscribe = Json_codec.required_as fields "subscribe" Json_codec.bool in
    let%map idempotency_key =
      Json_codec.required_as fields "idempotency_key" Idempotency_key.of_json
    in
    { spec; requested_mode; subscribe; idempotency_key }
  ;;
end

module List_request = struct
  type t =
    { page : Page.Request.t
    ; desired_state : desired_state option
    ; prompt_id : Id.Prompt_definition.t option
    ; workspace_id : Id.Workspace_definition.t option
    ; owner_principal_id : Id.Principal.t option
    ; labels : (string * string) list
    }
  [@@deriving sexp]

  let to_json t =
    let filters =
      [ optional_field "desired_state" t.desired_state (fun state ->
          `String (desired_state_to_string state))
      ; optional_field "prompt_id" t.prompt_id Id.Prompt_definition.to_json
      ; optional_field "workspace_id" t.workspace_id Id.Workspace_definition.to_json
      ; optional_field "owner_principal_id" t.owner_principal_id Id.Principal.to_json
      ; (if List.is_empty t.labels then None else Some ("labels", labels_to_json t.labels))
      ]
      |> List.filter_opt
    in
    `Object (Page.Request.to_fields t.page @ filters)
  ;;

  let decode_filters fields =
    let open Result.Let_syntax in
    let%bind desired_state =
      Json_codec.optional_as fields "desired_state" desired_state_of_json
    in
    let%bind prompt_id =
      Json_codec.optional_as fields "prompt_id" Id.Prompt_definition.of_json
    in
    let%bind workspace_id =
      Json_codec.optional_as fields "workspace_id" Id.Workspace_definition.of_json
    in
    let%map owner_principal_id =
      Json_codec.optional_as fields "owner_principal_id" Id.Principal.of_json
    in
    desired_state, prompt_id, workspace_id, owner_principal_id
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind page = Page.Request.of_fields fields in
    let%bind desired_state, prompt_id, workspace_id, owner_principal_id =
      decode_filters fields
    in
    let%bind labels = Json_codec.optional_as fields "labels" labels_of_json in
    let%map labels = validate_labels (Option.value labels ~default:[]) in
    { page; desired_state; prompt_id; workspace_id; owner_principal_id; labels }
  ;;
end

module Get_request = struct
  type t =
    { session_id : Id.Session.t
    ; history : History.Window_request.t option
    }
  [@@deriving sexp]

  let to_json t =
    let fields = [ "session_id", Id.Session.to_json t.session_id ] in
    match t.history with
    | None -> `Object fields
    | Some history ->
      `Object (fields @ [ "history", History.Window_request.to_json history ])
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
    let%map history =
      Json_codec.optional_as fields "history" History.Window_request.of_json
    in
    { session_id; history }
  ;;
end

let decode_session_attachment fields =
  let open Result.Let_syntax in
  let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
  let%map attachment_id =
    Json_codec.required_as fields "attachment_id" Id.Attachment.of_json
  in
  session_id, attachment_id
;;

let decode_idempotency_key fields =
  Json_codec.required_as fields "idempotency_key" Idempotency_key.of_json
;;

let mutation_fields ~session_id ~attachment_id ~idempotency_key =
  [ "session_id", Id.Session.to_json session_id
  ; "attachment_id", Id.Attachment.to_json attachment_id
  ; "idempotency_key", Idempotency_key.to_json idempotency_key
  ]
;;

module Attach_request = struct
  type t =
    { session_id : Id.Session.t
    ; requested_mode : attachment_mode
    ; subscribe : bool
    ; after_sequence : int64 option
    ; reclaim_token : string option
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    let fields =
      [ Some ("session_id", Id.Session.to_json t.session_id)
      ; Some ("requested_mode", `String (attachment_mode_to_string t.requested_mode))
      ; Some ("subscribe", if t.subscribe then `True else `False)
      ; optional_field "after_sequence" t.after_sequence int64_to_json
      ; optional_field "reclaim_token" t.reclaim_token (fun value -> `String value)
      ; Some ("idempotency_key", Idempotency_key.to_json t.idempotency_key)
      ]
      |> List.filter_opt
    in
    `Object fields
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
    let%bind requested_mode =
      Json_codec.required_as fields "requested_mode" attachment_mode_of_json
    in
    let%bind subscribe = Json_codec.required_as fields "subscribe" Json_codec.bool in
    let%bind after_sequence =
      Json_codec.optional_as fields "after_sequence" nonnegative_int64
    in
    let%bind reclaim_token =
      Json_codec.optional_as fields "reclaim_token" Json_codec.string
    in
    if Option.exists reclaim_token ~f:(fun token -> String.length token > 512)
    then Error (Protocol_error.invalid_request "owner reclaim token is too long")
    else (
      let%map idempotency_key = decode_idempotency_key fields in
      { session_id
      ; requested_mode
      ; subscribe
      ; after_sequence
      ; reclaim_token
      ; idempotency_key
      })
  ;;
end

module Detach_request = struct
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      (mutation_fields
         ~session_id:t.session_id
         ~attachment_id:t.attachment_id
         ~idempotency_key:t.idempotency_key)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id, attachment_id = decode_session_attachment fields in
    let%map idempotency_key = decode_idempotency_key fields in
    { session_id; attachment_id; idempotency_key }
  ;;
end

module Renew_owner_request = struct
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; lease_generation : int64
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      (mutation_fields
         ~session_id:t.session_id
         ~attachment_id:t.attachment_id
         ~idempotency_key:t.idempotency_key
       @ [ "lease_generation", int64_to_json t.lease_generation ])
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id, attachment_id = decode_session_attachment fields in
    let%bind lease_generation =
      Json_codec.required_as fields "lease_generation" nonnegative_int64
    in
    let%map idempotency_key = decode_idempotency_key fields in
    { session_id; attachment_id; lease_generation; idempotency_key }
  ;;
end

module Start_request = struct
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; queue_if_limited : bool
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      (mutation_fields
         ~session_id:t.session_id
         ~attachment_id:t.attachment_id
         ~idempotency_key:t.idempotency_key
       @ [ ("queue_if_limited", if t.queue_if_limited then `True else `False) ])
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id, attachment_id = decode_session_attachment fields in
    let%bind queue_if_limited =
      Json_codec.required_as fields "queue_if_limited" Json_codec.bool
    in
    let%map idempotency_key = decode_idempotency_key fields in
    { session_id; attachment_id; queue_if_limited; idempotency_key }
  ;;
end

module Stop_request = struct
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; mode : stop_mode
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      (mutation_fields
         ~session_id:t.session_id
         ~attachment_id:t.attachment_id
         ~idempotency_key:t.idempotency_key
       @ [ "mode", `String (stop_mode_to_string t.mode) ])
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id, attachment_id = decode_session_attachment fields in
    let%bind mode = Json_codec.required_as fields "mode" stop_mode_of_json in
    let%map idempotency_key = decode_idempotency_key fields in
    { session_id; attachment_id; mode; idempotency_key }
  ;;
end

module Cancel_operation_request = struct
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; operation_id : Id.Operation.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      (mutation_fields
         ~session_id:t.session_id
         ~attachment_id:t.attachment_id
         ~idempotency_key:t.idempotency_key
       @ [ "operation_id", Id.Operation.to_json t.operation_id ])
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id, attachment_id = decode_session_attachment fields in
    let%bind operation_id =
      Json_codec.required_as fields "operation_id" Id.Operation.of_json
    in
    let%map idempotency_key = decode_idempotency_key fields in
    { session_id; attachment_id; operation_id; idempotency_key }
  ;;
end

module Message_content = struct
  type kind =
    | Plain_text
    | Chatmd
  [@@deriving compare, equal, sexp]

  type t =
    { kind : kind
    ; text : string
    ; attachments : Blob.Input.t list
    }
  [@@deriving sexp]

  let kind_to_string = function
    | Plain_text -> "plain_text"
    | Chatmd -> "chatmd"
  ;;

  let kind_of_json =
    Json_codec.enum
      ~name:"message content kind"
      [ "plain_text", Plain_text; "chatmd", Chatmd ]
  ;;

  let to_json t =
    `Object
      [ "kind", `String (kind_to_string t.kind)
      ; "text", `String t.text
      ; "attachments", `Array (List.map t.attachments ~f:Blob.Input.to_json)
      ]
  ;;

  let validate t =
    let attachments =
      List.map t.attachments ~f:(fun attachment ->
        Json_codec.canonical_string (Blob.Input.to_json attachment))
    in
    if String.is_empty (String.strip t.text)
    then Error (Protocol_error.invalid_request "message text must be nonempty")
    else (
      match Result.all attachments with
      | Error _ ->
        Error (Protocol_error.invalid_request "message attachment is not canonicalizable")
      | Ok attachments ->
        if Option.is_some (List.find_a_dup attachments ~compare:String.compare)
        then
          Error (Protocol_error.invalid_request "message attachments contain duplicates")
        else Ok t)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind kind = Json_codec.required_as fields "kind" kind_of_json in
    let%bind text = Json_codec.required_as fields "text" Json_codec.string in
    let%bind attachments =
      Json_codec.required_as fields "attachments" (Json_codec.list Blob.Input.of_json)
    in
    validate { kind; text; attachments }
  ;;
end

module Send_message_request = struct
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; content : Message_content.t
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      (mutation_fields
         ~session_id:t.session_id
         ~attachment_id:t.attachment_id
         ~idempotency_key:t.idempotency_key
       @ [ "content", Message_content.to_json t.content ])
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id, attachment_id = decode_session_attachment fields in
    let%bind content =
      match Json_codec.optional fields "content", Json_codec.optional fields "text" with
      | Some content, None -> Message_content.of_json content
      | None, Some (`String text) ->
        Message_content.validate { kind = Plain_text; text; attachments = [] }
      | None, Some _ ->
        Error (Protocol_error.invalid_request "message text must be a JSON string")
      | Some _, Some _ ->
        Error (Protocol_error.invalid_request "provide either content or text, not both")
      | None, None -> Error (Protocol_error.invalid_request "message content is required")
    in
    let%map idempotency_key = decode_idempotency_key fields in
    { session_id; attachment_id; content; idempotency_key }
  ;;
end

module Compact_request = struct
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_revision : int64 option
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    let fields =
      mutation_fields
        ~session_id:t.session_id
        ~attachment_id:t.attachment_id
        ~idempotency_key:t.idempotency_key
    in
    let fields =
      match t.expected_revision with
      | None -> fields
      | Some revision -> fields @ [ "expected_revision", int64_to_json revision ]
    in
    `Object fields
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id, attachment_id = decode_session_attachment fields in
    let%bind expected_revision =
      Json_codec.optional_as fields "expected_revision" nonnegative_int64
    in
    let%map idempotency_key = decode_idempotency_key fields in
    { session_id; attachment_id; expected_revision; idempotency_key }
  ;;
end

module Delete_history_request = struct
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; history_id : History.Id.t
    ; expected_revision : int64
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      (mutation_fields
         ~session_id:t.session_id
         ~attachment_id:t.attachment_id
         ~idempotency_key:t.idempotency_key
       @ [ "history_id", History.Id.to_json t.history_id
         ; "expected_revision", int64_to_json t.expected_revision
         ])
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id, attachment_id = decode_session_attachment fields in
    let%bind history_id = Json_codec.required_as fields "history_id" History.Id.of_json in
    let%bind expected_revision =
      Json_codec.required_as fields "expected_revision" nonnegative_int64
    in
    let%map idempotency_key = decode_idempotency_key fields in
    { session_id; attachment_id; history_id; expected_revision; idempotency_key }
  ;;
end

module Export_request = struct
  type format =
    | Chatmd
    | Json
  [@@deriving compare, equal, sexp]

  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; format : format
    ; revision : int64 option
    ; history : History.Window_request.t option
    }
  [@@deriving sexp]

  let format_to_string = function
    | Chatmd -> "chatmd"
    | Json -> "json"
  ;;

  let format_of_json =
    Json_codec.enum ~name:"export format" [ "chatmd", Chatmd; "json", Json ]
  ;;

  let to_json t =
    let fields =
      [ Some ("session_id", Id.Session.to_json t.session_id)
      ; Some ("attachment_id", Id.Attachment.to_json t.attachment_id)
      ; Some ("format", `String (format_to_string t.format))
      ; optional_field "revision" t.revision int64_to_json
      ; optional_field "history" t.history History.Window_request.to_json
      ]
      |> List.filter_opt
    in
    `Object fields
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id, attachment_id = decode_session_attachment fields in
    let%bind format = Json_codec.required_as fields "format" format_of_json in
    let%bind revision = Json_codec.optional_as fields "revision" nonnegative_int64 in
    let%map history =
      Json_codec.optional_as fields "history" History.Window_request.of_json
    in
    { session_id; attachment_id; format; revision; history }
  ;;
end

let decode_expected_mutation fields =
  let open Result.Let_syntax in
  let%bind session_id, attachment_id = decode_session_attachment fields in
  let%bind expected_revision =
    Json_codec.required_as fields "expected_revision" nonnegative_int64
  in
  let%map idempotency_key = decode_idempotency_key fields in
  session_id, attachment_id, expected_revision, idempotency_key
;;

module Reset_request = struct
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_revision : int64
    ; keep_history : bool
    ; keep_tasks : bool
    ; keep_cache : bool
    ; keep_workspace : bool
    ; keep_grants : bool
    ; keep_labels : bool
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let option_fields t =
    [ ("keep_history", if t.keep_history then `True else `False)
    ; ("keep_tasks", if t.keep_tasks then `True else `False)
    ; ("keep_cache", if t.keep_cache then `True else `False)
    ; ("keep_workspace", if t.keep_workspace then `True else `False)
    ; ("keep_grants", if t.keep_grants then `True else `False)
    ; ("keep_labels", if t.keep_labels then `True else `False)
    ]
  ;;

  let to_json t =
    let fields =
      mutation_fields
        ~session_id:t.session_id
        ~attachment_id:t.attachment_id
        ~idempotency_key:t.idempotency_key
      @ [ "expected_revision", int64_to_json t.expected_revision ]
      @ option_fields t
    in
    `Object fields
  ;;

  let decode_options fields =
    let open Result.Let_syntax in
    let%bind keep_history =
      Json_codec.required_as fields "keep_history" Json_codec.bool
    in
    let%bind keep_tasks = Json_codec.required_as fields "keep_tasks" Json_codec.bool in
    let%bind keep_cache = Json_codec.required_as fields "keep_cache" Json_codec.bool in
    let%bind keep_workspace =
      Json_codec.required_as fields "keep_workspace" Json_codec.bool
    in
    let%bind keep_grants = Json_codec.required_as fields "keep_grants" Json_codec.bool in
    let%map keep_labels = Json_codec.required_as fields "keep_labels" Json_codec.bool in
    keep_history, keep_tasks, keep_cache, keep_workspace, keep_grants, keep_labels
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id, attachment_id, expected_revision, idempotency_key =
      decode_expected_mutation fields
    in
    let%map keep_history, keep_tasks, keep_cache, keep_workspace, keep_grants, keep_labels
      =
      decode_options fields
    in
    { session_id
    ; attachment_id
    ; expected_revision
    ; keep_history
    ; keep_tasks
    ; keep_cache
    ; keep_workspace
    ; keep_grants
    ; keep_labels
    ; idempotency_key
    }
  ;;
end

module Rebuild_request = struct
  type prompt_choice =
    | Pinned
    | Current_catalog
  [@@deriving compare, equal, sexp]

  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_revision : int64
    ; prompt_choice : prompt_choice
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let prompt_choice_to_string = function
    | Pinned -> "pinned"
    | Current_catalog -> "current_catalog"
  ;;

  let prompt_choice_of_json =
    Json_codec.enum
      ~name:"rebuild prompt choice"
      [ "pinned", Pinned; "current_catalog", Current_catalog ]
  ;;

  let to_json t =
    `Object
      (mutation_fields
         ~session_id:t.session_id
         ~attachment_id:t.attachment_id
         ~idempotency_key:t.idempotency_key
       @ [ "expected_revision", int64_to_json t.expected_revision
         ; "prompt_choice", `String (prompt_choice_to_string t.prompt_choice)
         ])
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id, attachment_id, expected_revision, idempotency_key =
      decode_expected_mutation fields
    in
    let%map prompt_choice =
      Json_codec.required_as fields "prompt_choice" prompt_choice_of_json
    in
    { session_id; attachment_id; expected_revision; prompt_choice; idempotency_key }
  ;;
end

module Upgrade_prompt_request = struct
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_revision : int64
    ; target_revision : Id.Prompt_revision.t
    ; allow_migration : bool
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      (mutation_fields
         ~session_id:t.session_id
         ~attachment_id:t.attachment_id
         ~idempotency_key:t.idempotency_key
       @ [ "expected_revision", int64_to_json t.expected_revision
         ; "target_revision", Id.Prompt_revision.to_json t.target_revision
         ; ("allow_migration", if t.allow_migration then `True else `False)
         ])
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id, attachment_id, expected_revision, idempotency_key =
      decode_expected_mutation fields
    in
    let%bind target_revision =
      Json_codec.required_as fields "target_revision" Id.Prompt_revision.of_json
    in
    let%map allow_migration =
      Json_codec.required_as fields "allow_migration" Json_codec.bool
    in
    { session_id
    ; attachment_id
    ; expected_revision
    ; target_revision
    ; allow_migration
    ; idempotency_key
    }
  ;;
end

module Delete_request = struct
  type policy =
    | Archive
    | Remove
  [@@deriving compare, equal, sexp]

  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; expected_revision : int64
    ; policy : policy
    ; confirmation : string
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  let policy_to_string = function
    | Archive -> "archive"
    | Remove -> "remove"
  ;;

  let policy_of_json =
    Json_codec.enum
      ~name:"session deletion policy"
      [ "archive", Archive; "remove", Remove ]
  ;;

  let to_json t =
    `Object
      (mutation_fields
         ~session_id:t.session_id
         ~attachment_id:t.attachment_id
         ~idempotency_key:t.idempotency_key
       @ [ "expected_revision", int64_to_json t.expected_revision
         ; "policy", `String (policy_to_string t.policy)
         ; "confirmation", `String t.confirmation
         ])
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id, attachment_id, expected_revision, idempotency_key =
      decode_expected_mutation fields
    in
    let%bind policy = Json_codec.required_as fields "policy" policy_of_json in
    let%bind confirmation =
      Json_codec.required_as fields "confirmation" Json_codec.string
    in
    if String.is_empty confirmation
    then Error (Protocol_error.invalid_request "deletion confirmation must be nonempty")
    else
      Ok
        { session_id
        ; attachment_id
        ; expected_revision
        ; policy
        ; confirmation
        ; idempotency_key
        }
  ;;
end
