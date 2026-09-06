open Core

let optional_field name value encode =
  Option.map value ~f:(fun value -> name, encode value)
;;

let nonnegative_int64 = Json_codec.bounded_int64 ~min:Int64.zero ~max:Int64.max_value
let int64_to_json value = `Number (Int64.to_string value)

module Server_info = struct
  type t =
    { server_id : Id.Server.t
    ; implementation : Initialize.Implementation.t
    ; protocol_version : Version.t
    ; features : string list
    ; transports : string list
    ; limits : Initialize.Limits.t
    ; unsafe_development_auth : bool
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "server_id", Id.Server.to_json t.server_id
      ; "implementation", Initialize.Implementation.to_json t.implementation
      ; "protocol_version", Version.to_json t.protocol_version
      ; "features", `Array (List.map t.features ~f:(fun value -> `String value))
      ; "transports", `Array (List.map t.transports ~f:(fun value -> `String value))
      ; "limits", Initialize.Limits.to_json t.limits
      ; ("unsafe_development_auth", if t.unsafe_development_auth then `True else `False)
      ]
  ;;

  let decode_lists fields =
    let open Result.Let_syntax in
    let%bind features =
      Json_codec.required_as fields "features" (Json_codec.list Json_codec.string)
    in
    let%map transports =
      Json_codec.required_as fields "transports" (Json_codec.list Json_codec.string)
    in
    features, transports
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind server_id = Json_codec.required_as fields "server_id" Id.Server.of_json in
    let%bind implementation =
      Json_codec.required_as fields "implementation" Initialize.Implementation.of_json
    in
    let%bind protocol_version =
      Json_codec.required_as fields "protocol_version" Version.of_json
    in
    let%bind features, transports = decode_lists fields in
    let%bind limits = Json_codec.required_as fields "limits" Initialize.Limits.of_json in
    let%map unsafe_development_auth =
      Json_codec.required_as fields "unsafe_development_auth" Json_codec.bool
    in
    { server_id
    ; implementation
    ; protocol_version
    ; features
    ; transports
    ; limits
    ; unsafe_development_auth
    }
  ;;
end

module Session_mutation = struct
  type t =
    { session : Session.t
    ; mutation : Mutation_result.t
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      (("session", Session.to_json t.session) :: Mutation_result.to_fields t.mutation)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session = Json_codec.required_as fields "session" Session.of_json in
    let%map mutation = Mutation_result.of_fields fields in
    { session; mutation }
  ;;
end

module Attach = struct
  type replay =
    | Current
    | Events of Event.Durable.t list
    | Snapshot of Snapshot.t
  [@@deriving sexp]

  type t =
    { attachment : Session.Attachment.t
    ; replay : replay
    ; latest_event_sequence : int64
    ; reclaim_token : string option
    }
  [@@deriving sexp]

  let replay_to_json = function
    | Current -> `Object [ "type", `String "current" ]
    | Events events ->
      `Object
        [ "type", `String "events"
        ; "events", `Array (List.map events ~f:Event.Durable.to_json)
        ]
    | Snapshot snapshot ->
      `Object [ "type", `String "snapshot"; "snapshot", Snapshot.to_json snapshot ]
  ;;

  let replay_of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind encoded = Json_codec.required_as fields "type" Json_codec.string in
    match encoded with
    | "current" -> Ok Current
    | "events" ->
      Result.map
        (Json_codec.required_as fields "events" (Json_codec.list Event.Durable.of_json))
        ~f:(fun events -> Events events)
    | "snapshot" ->
      Result.map (Json_codec.required_as fields "snapshot" Snapshot.of_json) ~f:(fun x ->
        Snapshot x)
    | _ -> Error (Protocol_error.invalid_request "unknown attach replay disposition")
  ;;

  let to_json t =
    let fields =
      [ Some ("attachment", Session.Attachment.to_json t.attachment)
      ; Some ("replay", replay_to_json t.replay)
      ; Some ("latest_event_sequence", int64_to_json t.latest_event_sequence)
      ; Option.map t.reclaim_token ~f:(fun value -> "reclaim_token", `String value)
      ]
      |> List.filter_opt
    in
    `Object fields
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind attachment =
      Json_codec.required_as fields "attachment" Session.Attachment.of_json
    in
    let%bind replay = Json_codec.required_as fields "replay" replay_of_json in
    let%bind latest_event_sequence =
      Json_codec.required_as fields "latest_event_sequence" nonnegative_int64
    in
    let%map reclaim_token =
      Json_codec.optional_as fields "reclaim_token" Json_codec.string
    in
    { attachment; replay; latest_event_sequence; reclaim_token }
  ;;
end

module Create = struct
  type t =
    { session : Session.t
    ; mutation : Mutation_result.t
    ; attachment : Attach.t option
    }
  [@@deriving sexp]

  let to_json t =
    let fields =
      ("session", Session.to_json t.session) :: Mutation_result.to_fields t.mutation
    in
    match t.attachment with
    | None -> `Object fields
    | Some attachment -> `Object (fields @ [ "attachment", Attach.to_json attachment ])
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session = Json_codec.required_as fields "session" Session.of_json in
    let%bind mutation = Mutation_result.of_fields fields in
    let%map attachment = Json_codec.optional_as fields "attachment" Attach.of_json in
    { session; mutation; attachment }
  ;;
end

module Send_message = struct
  type disposition =
    | Started
    | Deferred
  [@@deriving compare, equal, sexp]

  type t =
    { history_id : History.Id.t
    ; disposition : disposition
    ; operation_id : Id.Operation.t option
    ; mutation : Mutation_result.t
    }
  [@@deriving sexp]

  let disposition_to_string = function
    | Started -> "started"
    | Deferred -> "deferred"
  ;;

  let disposition_of_json =
    Json_codec.enum
      ~name:"message disposition"
      [ "started", Started; "deferred", Deferred ]
  ;;

  let to_json t =
    let fields =
      [ Some ("history_id", History.Id.to_json t.history_id)
      ; Some ("disposition", `String (disposition_to_string t.disposition))
      ; optional_field "operation_id" t.operation_id Id.Operation.to_json
      ]
      |> List.filter_opt
    in
    `Object (fields @ Mutation_result.to_fields t.mutation)
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind history_id = Json_codec.required_as fields "history_id" History.Id.of_json in
    let%bind disposition =
      Json_codec.required_as fields "disposition" disposition_of_json
    in
    let%bind operation_id =
      Json_codec.optional_as fields "operation_id" Id.Operation.of_json
    in
    let%bind mutation = Mutation_result.of_fields fields in
    match disposition, operation_id with
    | Started, None ->
      Error (Protocol_error.invalid_request "started message lacks an operation ID")
    | Deferred, Some _ ->
      Error (Protocol_error.invalid_request "deferred message has an operation ID")
    | _ -> Ok { history_id; disposition; operation_id; mutation }
  ;;
end

module Export = struct
  type t =
    { blob : Blob.Metadata.t
    ; session_revision : int64
    ; latest_event_sequence : int64
    }
  [@@deriving sexp]

  let to_json t =
    `Object
      [ "blob", Blob.Metadata.to_json t.blob
      ; "session_revision", int64_to_json t.session_revision
      ; "latest_event_sequence", int64_to_json t.latest_event_sequence
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind blob = Json_codec.required_as fields "blob" Blob.Metadata.of_json in
    let%bind session_revision =
      Json_codec.required_as fields "session_revision" nonnegative_int64
    in
    let%map latest_event_sequence =
      Json_codec.required_as fields "latest_event_sequence" nonnegative_int64
    in
    { blob; session_revision; latest_event_sequence }
  ;;
end

module Delete = struct
  type t =
    { session_id : Id.Session.t
    ; deleted_at : Timestamp.t
    ; archive : Blob.Metadata.t option
    }
  [@@deriving sexp]

  let to_json t =
    let fields =
      [ Some ("session_id", Id.Session.to_json t.session_id)
      ; Some ("deleted_at", Timestamp.to_json t.deleted_at)
      ; optional_field "archive" t.archive Blob.Metadata.to_json
      ]
      |> List.filter_opt
    in
    `Object fields
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
    let%bind deleted_at = Json_codec.required_as fields "deleted_at" Timestamp.of_json in
    let%map archive = Json_codec.optional_as fields "archive" Blob.Metadata.of_json in
    { session_id; deleted_at; archive }
  ;;
end

type t =
  | Protocol_initialize of Initialize.Response.t
  | Protocol_ping of Ping.Response.t
  | Server_info of Server_info.t
  | Server_health of Health.Response.t
  | Prompt_list of Prompt.t Page.t
  | Prompt_get of Prompt.t
  | Workspace_list of Workspace.t Page.t
  | Workspace_get of Workspace.t
  | Blob_read of Blob.Chunk.t
  | Session_create of Create.t
  | Session_list of Session.t Page.t
  | Session_get of Snapshot.t
  | Session_attach of Attach.t
  | Session_detach of Mutation_result.t
  | Session_renew_owner of Session.Owner_lease.t * Mutation_result.t
  | Session_start of Session_mutation.t
  | Session_stop of Session_mutation.t
  | Session_cancel_operation of Session_mutation.t
  | Session_send_message of Send_message.t
  | Session_compact of Session_mutation.t
  | Session_delete_history of Session_mutation.t
  | Session_export of Export.t
  | Session_reset of Session_mutation.t
  | Session_rebuild of Session_mutation.t
  | Session_upgrade_prompt of Session_mutation.t
  | Session_delete of Delete.t
  | Permission_list of Permission.t Page.t
  | Permission_respond of Permission.Respond_result.t
  | Grant_list of Grant.t Page.t
  | Grant_revoke of Grant.Revoke_result.t
  | Audit_read of Audit.t Page.t
  | Job_list of Job.t Page.t
  | Job_get of Job.t
  | Job_cancel of Job.Cancel_result.t
  | Schedule_list of Schedule.t Page.t
  | Schedule_get of Schedule.t
  | Schedule_create of Schedule.Mutation_response.t
  | Schedule_cancel of Schedule.Mutation_response.t
[@@deriving sexp]

let method_name = function
  | Protocol_initialize _ -> "protocol.initialize"
  | Protocol_ping _ -> "protocol.ping"
  | Server_info _ -> "server.info"
  | Server_health _ -> "server.health"
  | Prompt_list _ -> "prompt.list"
  | Prompt_get _ -> "prompt.get"
  | Workspace_list _ -> "workspace.list"
  | Workspace_get _ -> "workspace.get"
  | Blob_read _ -> "blob.read"
  | Session_create _ -> "session.create"
  | Session_list _ -> "session.list"
  | Session_get _ -> "session.get"
  | Session_attach _ -> "session.attach"
  | Session_detach _ -> "session.detach"
  | Session_renew_owner _ -> "session.renew_owner"
  | Session_start _ -> "session.start"
  | Session_stop _ -> "session.stop"
  | Session_cancel_operation _ -> "session.cancel_operation"
  | Session_send_message _ -> "session.send_message"
  | Session_compact _ -> "session.compact"
  | Session_delete_history _ -> "session.delete_history"
  | Session_export _ -> "session.export"
  | Session_reset _ -> "session.reset"
  | Session_rebuild _ -> "session.rebuild"
  | Session_upgrade_prompt _ -> "session.upgrade_prompt"
  | Session_delete _ -> "session.delete"
  | Permission_list _ -> "permission.list"
  | Permission_respond _ -> "permission.respond"
  | Grant_list _ -> "grant.list"
  | Grant_revoke _ -> "grant.revoke"
  | Audit_read _ -> "audit.read"
  | Job_list _ -> "job.list"
  | Job_get _ -> "job.get"
  | Job_cancel _ -> "job.cancel"
  | Schedule_list _ -> "schedule.list"
  | Schedule_get _ -> "schedule.get"
  | Schedule_create _ -> "schedule.create"
  | Schedule_cancel _ -> "schedule.cancel"
;;

let to_json = function
  | Protocol_initialize value -> Initialize.Response.to_json value
  | Protocol_ping value -> Ping.Response.to_json value
  | Server_info value -> Server_info.to_json value
  | Server_health value -> Health.Response.to_json value
  | Prompt_list value -> Page.to_json Prompt.to_json value
  | Prompt_get value -> Prompt.to_json value
  | Workspace_list value -> Page.to_json Workspace.to_json value
  | Workspace_get value -> Workspace.to_json value
  | Blob_read value -> Blob.Chunk.to_json value
  | Session_create value -> Create.to_json value
  | Session_list value -> Page.to_json Session.to_json value
  | Session_get value -> Snapshot.to_json value
  | Session_attach value -> Attach.to_json value
  | Session_detach value -> Mutation_result.to_json value
  | Session_renew_owner (lease, mutation) ->
    `Object
      (("owner_lease", Session.Owner_lease.to_json lease)
       :: Mutation_result.to_fields mutation)
  | Session_start value
  | Session_stop value
  | Session_cancel_operation value
  | Session_compact value
  | Session_delete_history value
  | Session_reset value
  | Session_rebuild value
  | Session_upgrade_prompt value -> Session_mutation.to_json value
  | Session_send_message value -> Send_message.to_json value
  | Session_export value -> Export.to_json value
  | Session_delete value -> Delete.to_json value
  | Permission_list value -> Page.to_json Permission.to_json value
  | Permission_respond value -> Permission.Respond_result.to_json value
  | Grant_list value -> Page.to_json Grant.to_json value
  | Grant_revoke value -> Grant.Revoke_result.to_json value
  | Audit_read value -> Page.to_json Audit.to_json value
  | Job_list value -> Page.to_json Job.to_json value
  | Job_get value -> Job.to_json value
  | Job_cancel value -> Job.Cancel_result.to_json value
  | Schedule_list value -> Page.to_json Schedule.to_json value
  | Schedule_get value -> Schedule.to_json value
  | Schedule_create value | Schedule_cancel value ->
    Schedule.Mutation_response.to_json value
;;

let renew_owner_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind lease =
    Json_codec.required_as fields "owner_lease" Session.Owner_lease.of_json
  in
  let%map mutation = Mutation_result.of_fields fields in
  Session_renew_owner (lease, mutation)
;;

let map decode wrap json = Result.map (decode json) ~f:wrap

let decoders =
  [ ( "protocol.initialize"
    , map Initialize.Response.of_json (fun x -> Protocol_initialize x) )
  ; "protocol.ping", map Ping.Response.of_json (fun x -> Protocol_ping x)
  ; "server.info", map Server_info.of_json (fun x -> Server_info x)
  ; "server.health", map Health.Response.of_json (fun x -> Server_health x)
  ; "prompt.list", map (Page.of_json Prompt.of_json) (fun x -> Prompt_list x)
  ; "prompt.get", map Prompt.of_json (fun x -> Prompt_get x)
  ; "workspace.list", map (Page.of_json Workspace.of_json) (fun x -> Workspace_list x)
  ; "workspace.get", map Workspace.of_json (fun x -> Workspace_get x)
  ; "blob.read", map Blob.Chunk.of_json (fun x -> Blob_read x)
  ; "session.create", map Create.of_json (fun x -> Session_create x)
  ; "session.list", map (Page.of_json Session.of_json) (fun x -> Session_list x)
  ; "session.get", map Snapshot.of_json (fun x -> Session_get x)
  ; "session.attach", map Attach.of_json (fun x -> Session_attach x)
  ; "session.detach", map Mutation_result.of_json (fun x -> Session_detach x)
  ; "session.renew_owner", renew_owner_of_json
  ; "session.start", map Session_mutation.of_json (fun x -> Session_start x)
  ; "session.stop", map Session_mutation.of_json (fun x -> Session_stop x)
  ; ( "session.cancel_operation"
    , map Session_mutation.of_json (fun x -> Session_cancel_operation x) )
  ; "session.send_message", map Send_message.of_json (fun x -> Session_send_message x)
  ; "session.compact", map Session_mutation.of_json (fun x -> Session_compact x)
  ; ( "session.delete_history"
    , map Session_mutation.of_json (fun x -> Session_delete_history x) )
  ; "session.export", map Export.of_json (fun x -> Session_export x)
  ; "session.reset", map Session_mutation.of_json (fun x -> Session_reset x)
  ; "session.rebuild", map Session_mutation.of_json (fun x -> Session_rebuild x)
  ; ( "session.upgrade_prompt"
    , map Session_mutation.of_json (fun x -> Session_upgrade_prompt x) )
  ; "session.delete", map Delete.of_json (fun x -> Session_delete x)
  ; "permission.list", map (Page.of_json Permission.of_json) (fun x -> Permission_list x)
  ; ( "permission.respond"
    , map Permission.Respond_result.of_json (fun x -> Permission_respond x) )
  ; "grant.list", map (Page.of_json Grant.of_json) (fun x -> Grant_list x)
  ; "grant.revoke", map Grant.Revoke_result.of_json (fun x -> Grant_revoke x)
  ; "audit.read", map (Page.of_json Audit.of_json) (fun x -> Audit_read x)
  ; "job.list", map (Page.of_json Job.of_json) (fun x -> Job_list x)
  ; "job.get", map Job.of_json (fun x -> Job_get x)
  ; "job.cancel", map Job.Cancel_result.of_json (fun x -> Job_cancel x)
  ; "schedule.list", map (Page.of_json Schedule.of_json) (fun x -> Schedule_list x)
  ; "schedule.get", map Schedule.of_json (fun x -> Schedule_get x)
  ; "schedule.create", map Schedule.Mutation_response.of_json (fun x -> Schedule_create x)
  ; "schedule.cancel", map Schedule.Mutation_response.of_json (fun x -> Schedule_cancel x)
  ]
;;

let of_json ~method_ json =
  match List.Assoc.find decoders method_ ~equal:String.equal with
  | Some decode -> decode json
  | None ->
    Error
      (Protocol_error.create
         Protocol_error.Method_not_found
         ~message:("unknown protocol method result: " ^ method_)
         ~retryable:false
         ())
;;

let supported_methods = List.map decoders ~f:fst
