open! Core

let nonnegative_int64 = Json_codec.bounded_int64 ~min:Int64.zero ~max:Int64.max_value
let int64_to_json value = `Number (Int64.to_string value)

module Non_history = struct
  type t = Method_result.t [@@deriving sexp_of]

  let of_internal value =
    match value with
    | Method_result.Provider_login_challenge _
    | Session_get _
    | Session_create _
    | Session_attach _ ->
      Error
        (Protocol_error.invalid_request
           "response requires an explicit history or private projection")
    | Method_result.Provider_setup _
    | Provider_status _
    | Provider_login_begin _
    | Provider_login_cancel _
    | Provider_logout _
    | Provider_select _
    | Provider_configure_environment _
    | Method_result.Protocol_initialize _
    | Protocol_ping _
    | Server_info _
    | Server_health _
    | Prompt_list _
    | Prompt_get _
    | Workspace_list _
    | Workspace_get _
    | Blob_read _
    | Session_list _
    | Session_configuration_get _
    | Session_configuration_update _
    | Session_inference_summary _
    | Session_inference_observations _
    | Session_detach _
    | Session_renew_owner _
    | Session_start _
    | Project_create _
    | Project_get _
    | Project_list _
    | Project_update _
    | Project_delete _
    | Collection_create _
    | Collection_get _
    | Collection_list _
    | Collection_update _
    | Collection_delete _
    | Session_update_metadata _
    | Session_update_organization _
    | Session_stop _
    | Session_cancel_operation _
    | Session_send_message _
    | Session_compact _
    | Session_edit_history _
    | Session_continue_history _
    | Session_delete_history _
    | Session_export _
    | Session_reset _
    | Session_rebuild _
    | Session_upgrade_prompt _
    | Session_delete _
    | Permission_list _
    | Permission_respond _
    | Grant_list _
    | Grant_revoke _
    | Audit_read _
    | Job_list _
    | Job_get _
    | Job_cancel _
    | Schedule_list _
    | Schedule_get _
    | Schedule_create _
    | Schedule_cancel _
    | Command_receipt _
    | Ingress_submit _ -> Ok value
  ;;

  let value t = t
end

module Attach = struct
  type replay =
    | Current
    | Events of Public_durable_event.t list
    | Snapshot of Public_snapshot.t
  [@@deriving sexp_of]

  type t =
    { attachment : Session.Attachment.t
    ; replay : replay
    ; latest_event_sequence : int64
    ; reclaim_token : string option
    }
  [@@deriving sexp_of]

  let replay_to_json = function
    | Current -> `Object [ "type", `String "current" ]
    | Events events ->
      `Object
        [ "type", `String "events"
        ; "events", `Array (List.map events ~f:Public_durable_event.to_json)
        ]
    | Snapshot snapshot ->
      `Object [ "type", `String "snapshot"; "snapshot", Public_snapshot.to_json snapshot ]
  ;;

  let replay_of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind encoded = Json_codec.required_as fields "type" Json_codec.string in
    match encoded with
    | "current" -> Ok Current
    | "events" ->
      Result.map
        (Json_codec.required_as
           fields
           "events"
           (Json_codec.list Public_durable_event.of_json))
        ~f:(fun events -> Events events)
    | "snapshot" ->
      Result.map
        (Json_codec.required_as fields "snapshot" Public_snapshot.of_json)
        ~f:(fun x -> Snapshot x)
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
    let%bind reclaim_token =
      Json_codec.optional_as fields "reclaim_token" Json_codec.string
    in
    let%map () =
      match replay with
      | Current -> Ok ()
      | Snapshot snapshot ->
        let snapshot = Public_snapshot.fields snapshot in
        if
          Id.Session.equal attachment.session_id snapshot.session.id
          && Int64.equal latest_event_sequence snapshot.latest_event_sequence
        then Ok ()
        else Error (Protocol_error.invalid_request "attach snapshot anchor mismatch")
      | Events events ->
        if
          List.for_all events ~f:(fun event ->
            Id.Session.equal event.Public_durable_event.session_id attachment.session_id)
          && Option.value_map (List.last events) ~default:true ~f:(fun event ->
            Int64.equal event.sequence latest_event_sequence)
        then Ok ()
        else Error (Protocol_error.invalid_request "attach replay anchor mismatch")
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
  [@@deriving sexp_of]

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
    let%bind attachment = Json_codec.optional_as fields "attachment" Attach.of_json in
    let%map () =
      if
        Option.for_all attachment ~f:(fun attached ->
          Id.Session.equal attached.Attach.attachment.session_id session.Session.id)
      then Ok ()
      else Error (Protocol_error.invalid_request "create attachment session mismatch")
    in
    { session; mutation; attachment }
  ;;
end

type t =
  | Private_provider_challenge of Provider_operator.Private_challenge.t
  | Session_get of Public_snapshot.t
  | Session_attach of Attach.t
  | Session_create of Create.t
  | Non_history of Non_history.t
[@@deriving sexp_of]

let method_name = function
  | Private_provider_challenge _ -> "provider.login.challenge"
  | Session_get _ -> "session.get"
  | Session_attach _ -> "session.attach"
  | Session_create _ -> "session.create"
  | Non_history value -> Method_result.method_name (Non_history.value value)
;;

let to_json = function
  | Private_provider_challenge value ->
    Provider_operator.Private_challenge.Authorized_transport.to_json value
  | Session_get snapshot -> Public_snapshot.to_json snapshot
  | Session_attach attached -> Attach.to_json attached
  | Session_create created -> Create.to_json created
  | Non_history value -> Method_result.to_json (Non_history.value value)
;;

let of_json ~method_ json =
  let open Result.Let_syntax in
  let%bind () = Projection_codec.validate json in
  match method_ with
  | "provider.login.challenge" ->
    Result.map
      (Provider_operator.Private_challenge.Authorized_transport.of_json json)
      ~f:(fun value -> Private_provider_challenge value)
  | "session.get" ->
    Result.map (Public_snapshot.of_json json) ~f:(fun value -> Session_get value)
  | "session.attach" ->
    Result.map (Attach.of_json json) ~f:(fun value -> Session_attach value)
  | "session.create" ->
    Result.map (Create.of_json json) ~f:(fun value -> Session_create value)
  | _ ->
    let%bind internal = Method_result.of_json ~method_ json in
    let%map value = Non_history.of_internal internal in
    Non_history value
;;

let validate t =
  of_json ~method_:(method_name t) (to_json t) |> Result.map ~f:(fun _ -> ())
;;
