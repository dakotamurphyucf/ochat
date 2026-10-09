open! Core
module J = Json_codec
module Raw_operation = Operation

module Observed = struct
  type t =
    | Stopped
    | Queued_for_slot
    | Starting
    | Recovering
    | Idle
    | Running_turn
    | Compacting
    | Waiting_for_permission
    | Stopping
    | Failed
  [@@deriving compare, equal, sexp]

  let values =
    [ "stopped", Stopped
    ; "queued_for_slot", Queued_for_slot
    ; "starting", Starting
    ; "recovering", Recovering
    ; "idle", Idle
    ; "running_turn", Running_turn
    ; "compacting", Compacting
    ; "waiting_for_permission", Waiting_for_permission
    ; "stopping", Stopping
    ; "failed", Failed
    ]
  ;;

  let to_json = function
    | Stopped -> `String "stopped"
    | Queued_for_slot -> `String "queued_for_slot"
    | Starting -> `String "starting"
    | Recovering -> `String "recovering"
    | Idle -> `String "idle"
    | Running_turn -> `String "running_turn"
    | Compacting -> `String "compacting"
    | Waiting_for_permission -> `String "waiting_for_permission"
    | Stopping -> `String "stopping"
    | Failed -> `String "failed"
  ;;

  let of_json = J.enum ~name:"activity observed state" values

  let of_session = function
    | Session.Stopped -> Stopped
    | Queued_for_slot -> Queued_for_slot
    | Starting -> Starting
    | Recovering -> Recovering
    | Idle -> Idle
    | Running_turn _ -> Running_turn
    | Compacting _ -> Compacting
    | Waiting_for_permission _ -> Waiting_for_permission
    | Stopping -> Stopping
    | Failed _ -> Failed
  ;;
end

module Operation = struct
  module Status = struct
    type t =
      | Starting
      | Running
      | Cancelling
      | Completed
      | Failed
      | Cancelled
      | Interrupted
    [@@deriving compare, equal, sexp]

    let to_json = function
      | Starting -> `String "starting"
      | Running -> `String "running"
      | Cancelling -> `String "cancelling"
      | Completed -> `String "completed"
      | Failed -> `String "failed"
      | Cancelled -> `String "cancelled"
      | Interrupted -> `String "interrupted"
    ;;

    let of_json =
      J.enum
        ~name:"activity operation status"
        [ "starting", Starting
        ; "running", Running
        ; "cancelling", Cancelling
        ; "completed", Completed
        ; "failed", Failed
        ; "cancelled", Cancelled
        ; "interrupted", Interrupted
        ]
    ;;
  end

  type t =
    { id : Id.Operation.t
    ; generation : int
    ; kind : Raw_operation.kind
    ; status : Status.t
    }

  let reasons =
    [ "user_submit", Raw_operation.User_submit
    ; "moderator_request", Moderator_request
    ; "idle_followup", Idle_followup
    ; "recovery_retry", Recovery_retry
    ; "administrative", Administrative
    ]
  ;;

  let reason_to_string = function
    | Raw_operation.User_submit -> "user_submit"
    | Moderator_request -> "moderator_request"
    | Idle_followup -> "idle_followup"
    | Recovery_retry -> "recovery_retry"
    | Administrative -> "administrative"
  ;;

  let kind_to_json = function
    | Raw_operation.Compaction -> `Object [ "type", `String "compaction" ]
    | Turn reason ->
      `Object [ "type", `String "turn"; "reason", `String (reason_to_string reason) ]
  ;;

  let kind_of_json json =
    let open Result.Let_syntax in
    let%bind f = J.fields json in
    let%bind kind = J.required_as f "type" J.string in
    match kind with
    | "compaction" -> Ok Raw_operation.Compaction
    | "turn" ->
      let%map reason =
        J.required_as f "reason" (J.enum ~name:"operation reason" reasons)
      in
      Raw_operation.Turn reason
    | _ -> Error (Protocol_error.invalid_request "unsupported operation kind")
  ;;

  let to_json t =
    `Object
      [ "id", Id.Operation.to_json t.id
      ; "generation", `Number (Int.to_string t.generation)
      ; "kind", kind_to_json t.kind
      ; "status", Status.to_json t.status
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = J.fields json in
    let%bind id = J.required_as f "id" Id.Operation.of_json in
    let%bind generation =
      J.required_as f "generation" (J.bounded_int ~min:0 ~max:Int.max_value)
    in
    let%bind kind = J.required_as f "kind" kind_of_json in
    let%map status = J.required_as f "status" Status.of_json in
    { id; generation; kind; status }
  ;;

  let of_operation (t : Raw_operation.t) =
    let status =
      match t.state with
      | Starting -> Status.Starting
      | Running -> Running
      | Cancelling -> Cancelling
      | Completed -> Completed
      | Failed _ -> Failed
      | Cancelled -> Cancelled
      | Interrupted _ -> Interrupted
    in
    of_json (to_json { id = t.id; generation = t.generation; kind = t.kind; status })
  ;;

  let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

  let t_of_sexp sexp =
    match of_json (Jsonaf.t_of_sexp sexp) with
    | Ok t -> t
    | Error e -> Sexplib.Conv.of_sexp_error e.message sexp
  ;;
end

type t =
  { session : Session_ref.t
  ; display_name : string option
  ; labels : (string * string) list
  ; creator : Id.Principal.t option
  ; created_at : Timestamp.t
  ; updated_at : Timestamp.t
  ; generation : int
  ; revision : int64
  ; metadata_revision : int64
  ; latest_event_sequence : int64
  ; execution_host : Session.execution_host
  ; liveness : Session.liveness
  ; persistence : Session.persistence
  ; desired_state : Session.desired_state
  ; observed : Observed.t
  ; archived : bool
  ; effective_organization : Session_organization.Values.t
  ; active_owner_principal_id : Id.Principal.t option
  ; active_operation : Operation.t option
  }

let host_to_json = function
  | Session.Daemon -> `String "daemon"
  | Embedded -> `String "embedded"
;;

let host_of_json =
  J.enum
    ~name:"activity execution host"
    [ "daemon", Session.Daemon; "embedded", Embedded ]
;;

let persistence_to_json = function
  | Session.Durable -> `String "durable"
  | Transient -> `String "transient"
;;

let persistence_of_json =
  J.enum
    ~name:"activity persistence"
    [ "durable", Session.Durable; "transient", Transient ]
;;

let stop_mode_to_json = function
  | Session.Graceful -> `String "graceful"
  | Cancel -> `String "cancel"
;;

let stop_mode_of_json =
  J.enum ~name:"activity stop mode" [ "graceful", Session.Graceful; "cancel", Cancel ]
;;

let liveness_to_json = function
  | Session.Detached -> `Object [ "type", `String "detached" ]
  | Process_bound -> `Object [ "type", `String "process_bound" ]
  | Owner_bound { disconnect_grace_ms; stop_mode } ->
    `Object
      [ "type", `String "owner_bound"
      ; "disconnect_grace_ms", `Number (Int.to_string disconnect_grace_ms)
      ; "stop_mode", stop_mode_to_json stop_mode
      ]
;;

let liveness_of_json json =
  let open Result.Let_syntax in
  let%bind f = J.fields json in
  let%bind kind = J.required_as f "type" J.string in
  match kind with
  | "detached" -> Ok Session.Detached
  | "process_bound" -> Ok Session.Process_bound
  | "owner_bound" ->
    let%bind disconnect_grace_ms =
      J.required_as f "disconnect_grace_ms" (J.bounded_int ~min:0 ~max:Int.max_value)
    in
    let%map stop_mode = J.required_as f "stop_mode" stop_mode_of_json in
    Session.Owner_bound { disconnect_grace_ms; stop_mode }
  | _ -> Error (Protocol_error.invalid_request "unsupported activity liveness")
;;

let validate_policy
      (host : Session.execution_host)
      (liveness : Session.liveness)
      (persistence : Session.persistence)
  =
  match host, liveness, persistence with
  | Daemon, Detached, Durable
  | Daemon, Owner_bound _, (Durable | Transient)
  | Embedded, Process_bound, (Durable | Transient) -> Ok ()
  | Daemon, Detached, Transient
  | Daemon, Process_bound, (Durable | Transient)
  | Embedded, (Detached | Owner_bound _), (Durable | Transient) ->
    Error (Protocol_error.invalid_request "invalid activity lifecycle policy")
;;

let optional = Projection_codec.optional

let to_json t =
  `Object
    ([ "session", Session_ref.to_json t.session
     ; "labels", `Object (List.map t.labels ~f:(fun (key, value) -> key, `String value))
     ; "created_at", Timestamp.to_json t.created_at
     ; "updated_at", Timestamp.to_json t.updated_at
     ; "generation", `Number (Int.to_string t.generation)
     ; "revision", `String (Int64.to_string t.revision)
     ; "metadata_revision", `String (Int64.to_string t.metadata_revision)
     ; "latest_event_sequence", `String (Int64.to_string t.latest_event_sequence)
     ; "execution_host", host_to_json t.execution_host
     ; "liveness", liveness_to_json t.liveness
     ; "persistence", persistence_to_json t.persistence
     ; "desired_state", `String (Session.desired_state_to_string t.desired_state)
     ; "observed", Observed.to_json t.observed
     ; ("archived", if t.archived then `True else `False)
     ; ( "effective_organization"
       , Session_organization.Values.to_json t.effective_organization )
     ]
     @ optional "display_name" t.display_name (fun s -> `String s)
     @ optional "creator" t.creator Id.Principal.to_json
     @ optional
         "active_owner_principal_id"
         t.active_owner_principal_id
         Id.Principal.to_json
     @ optional "active_operation" t.active_operation Operation.to_json)
;;

let revision_of_json = function
  | `String encoded ->
    (match Int64.of_string_opt encoded with
     | Some value when Int64.(value >= 0L) && String.equal encoded (Int64.to_string value)
       -> Ok value
     | Some _ | None -> Error (Protocol_error.invalid_request "invalid activity revision"))
  | _ ->
    Error (Protocol_error.invalid_request "activity revision must be a decimal string")
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind f = J.fields json in
  let%bind session = J.required_as f "session" Session_ref.of_json in
  let%bind display_name = J.optional_as f "display_name" J.string in
  let%bind labels =
    J.required_as f "labels" (fun json ->
      let%bind fields = J.fields json in
      Result.all
        (List.map (J.to_alist fields) ~f:(fun (key, value) ->
           let%map value = J.string value in
           key, value)))
  in
  let%bind metadata = Session_metadata.Values.create ~display_name ~labels in
  let%bind creator = J.optional_as f "creator" Id.Principal.of_json in
  let%bind created_at = J.required_as f "created_at" Timestamp.of_json in
  let%bind updated_at = J.required_as f "updated_at" Timestamp.of_json in
  let%bind generation =
    J.required_as f "generation" (J.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%bind revision = J.required_as f "revision" revision_of_json in
  let%bind metadata_revision = J.required_as f "metadata_revision" revision_of_json in
  let%bind latest_event_sequence =
    J.required_as f "latest_event_sequence" revision_of_json
  in
  let%bind execution_host = J.required_as f "execution_host" host_of_json in
  let%bind liveness = J.required_as f "liveness" liveness_of_json in
  let%bind persistence = J.required_as f "persistence" persistence_of_json in
  let%bind () = validate_policy execution_host liveness persistence in
  let%bind desired_state =
    J.required_as f "desired_state" Session.desired_state_of_json
  in
  let%bind observed = J.required_as f "observed" Observed.of_json in
  let%bind archived = J.required_as f "archived" J.bool in
  let%bind effective_organization =
    J.required_as f "effective_organization" Session_organization.Values.of_json
  in
  let%bind active_owner_principal_id =
    J.optional_as f "active_owner_principal_id" Id.Principal.of_json
  in
  let%bind active_operation = J.optional_as f "active_operation" Operation.of_json in
  if Timestamp.compare updated_at created_at < 0
  then
    Error
      (Protocol_error.invalid_request "invalid activity summary counters or timestamps")
  else
    Ok
      { session
      ; display_name = metadata.display_name
      ; labels = metadata.labels
      ; creator
      ; created_at
      ; updated_at
      ; generation
      ; revision
      ; metadata_revision
      ; latest_event_sequence
      ; execution_host
      ; liveness
      ; persistence
      ; desired_state
      ; observed
      ; archived
      ; effective_organization
      ; active_owner_principal_id
      ; active_operation
      }
;;

let of_catalog (catalog : Session_catalog.t) ~server_id =
  let open Result.Let_syntax in
  let t = catalog.session in
  let%bind active_operation =
    match t.active_operation with
    | None -> Ok None
    | Some operation -> Result.map (Operation.of_operation operation) ~f:Option.some
  in
  of_json
    (to_json
       { session = Session_ref.create ~server_id ~session_id:t.id
       ; display_name = t.spec.display_name
       ; labels = t.spec.labels
       ; creator = t.creator
       ; created_at = t.created_at
       ; updated_at = t.updated_at
       ; generation = t.generation
       ; revision = t.revision
       ; metadata_revision = t.metadata_revision
       ; latest_event_sequence = t.latest_event_sequence
       ; execution_host = t.spec.execution_host
       ; liveness = t.spec.liveness
       ; persistence = t.spec.persistence
       ; desired_state = t.desired_state
       ; observed = Observed.of_session t.observed_state
       ; archived = catalog.archived
       ; effective_organization = catalog.effective_organization
       ; active_owner_principal_id = catalog.active_owner_principal_id
       ; active_operation
       })
;;

let sexp_of_t t = Jsonaf.sexp_of_t (to_json t)

let t_of_sexp sexp =
  match of_json (Jsonaf.t_of_sexp sexp) with
  | Ok t -> t
  | Error e -> Sexplib.Conv.of_sexp_error e.message sexp
;;
