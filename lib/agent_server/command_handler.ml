open! Core

type create_session =
  command_audit:string option
  -> principal:Agent_protocol.Principal.t
  -> Agent_protocol.Session.Create_request.t
  -> (Session_registry.entry, Agent_protocol.Error.t) result

type t =
  { sw : Eio.Switch.t
  ; env : Eio_unix.Stdenv.base
  ; registry : Session_registry.t
  ; prompts : Agent_session.Prompt_catalog.t
  ; workspaces : Agent_session.Workspace_catalog.t
  ; start_queue : Agent_session.Start_queue.t
  ; idempotency_store : Agent_store.Idempotency_store.t
  ; idempotency_mutex : Eio.Mutex.t
  ; pagination : Pagination.t
  ; audit_store : Agent_store.Audit_store.t
  ; blob_store : Agent_store.Blob_store.t
  ; session_store : Agent_store.Session_store.t
  ; initialize :
      principal:Agent_protocol.Principal.t
      -> Agent_protocol.Initialize.Request.t
      -> (Agent_protocol.Initialize.Response.t, Agent_protocol.Error.t) result
  ; ping : Agent_protocol.Ping.Request.t -> Agent_protocol.Ping.Response.t
  ; server_info : unit -> Agent_protocol.Method_result.Server_info.t
  ; server_health : Agent_protocol.Health.Request.t -> Agent_protocol.Health.Response.t
  ; cancel_job : Agent_protocol.Id.Job.t -> unit
  ; create_session : create_session
  ; prepare_administration :
      Session_registry.entry
      -> Agent_session.Session_state.t
      -> fresh_history:bool
      -> (Agent_session.Session_state.t, Agent_protocol.Error.t) result
  }

let create
      ~sw
      ~env
      ~registry
      ~prompts
      ~workspaces
      ~start_queue
      ~idempotency_store
      ~audit_store
      ~blob_store
      ~session_store
      ~initialize
      ~ping
      ~server_info
      ~server_health
      ~cancel_job
      ~create_session
      ~prepare_administration
  =
  { sw
  ; env
  ; registry
  ; prompts
  ; workspaces
  ; start_queue
  ; idempotency_store
  ; idempotency_mutex = Eio.Mutex.create ()
  ; pagination = Pagination.create ()
  ; audit_store
  ; blob_store
  ; session_store
  ; initialize
  ; ping
  ; server_info
  ; server_health
  ; cancel_job
  ; create_session
  ; prepare_administration
  }
;;

let error code message = Agent_protocol.Error.create code ~message ~retryable:false ()

let persistence_error failure =
  Agent_protocol.Error.create
    Persistence_error
    ~message:(Sexp.to_string_hum ([%sexp_of: Agent_store.Store_error.t] failure))
    ~retryable:true
    ()
;;

let workspace_unavailable failure =
  Agent_protocol.Error.create
    Workspace_unavailable
    ~message:(Sexp.to_string_hum ([%sexp_of: Agent_store.Store_error.t] failure))
    ~retryable:false
    ()
;;

let now t =
  Eio.Time.now (Eio.Stdenv.clock t.env)
  |> Time_ns.Span.of_sec
  |> Time_ns.of_span_since_epoch
  |> Agent_protocol.Timestamp.of_time_ns
;;

let workspace_lease_mode = function
  | Agent_session.Workspace_definition.Read_only | Shared_write ->
    Some Agent_session.Workspace_lease.Shared
  | Exclusive -> Some Agent_session.Workspace_lease.Exclusive
;;

let canonical_path path =
  try Eio_posix.Low_level.realpath path with
  | _ -> path
;;

let physical_workspace_roots t =
  Agent_session.Workspace_catalog.definitions t.workspaces
  |> List.filter_map ~f:(fun definition ->
    match definition.Agent_session.Workspace_definition.source with
    | Physical { configured_root } -> Some (canonical_path configured_root)
    | Temporary _ -> None)
;;

let cleanup_context t entry instance =
  let open Result.Let_syntax in
  let%bind handle =
    entry.Session_registry.store_handle
    |> Result.of_option ~error:(error Invalid_state "session has no durable workspace")
  in
  let path = instance.Agent_session.Workspace_instance.canonical_root.native_path in
  let%bind managed_root, expected_path =
    match instance.source_kind with
    | Temporary Agent_session.Workspace_definition.Session_dir ->
      Ok
        ( Agent_store.Session_store.Handle.directory handle
        , Agent_store.Session_store.Handle.workspace_directory handle )
    | Temporary System_tmp ->
      let expected_name = Agent_protocol.Id.Workspace_instance.to_string instance.id in
      if String.equal (Filename.basename path) expected_name
      then Ok (Filename.dirname path, path)
      else Error (error Invalid_state "system temporary workspace identity is invalid")
    | Physical | Current -> Error (error Invalid_state "workspace is not temporary")
  in
  let data_root =
    Agent_store.Session_store.data_root t.session_store
    |> Agent_store.Data_root.path
    |> canonical_path
  in
  let protected_roots =
    Agent_session.Workspace_cleanup.
      { data_root
      ; physical_workspaces = physical_workspace_roots t
      ; managed_roots = [ canonical_path managed_root ]
      }
  in
  Ok (handle, protected_roots, canonical_path expected_path)
;;

let has_active_workspace_lease entry ~conflict_domain:_ =
  Option.value_map
    entry.Session_registry.capacity
    ~default:false
    ~f:Session_capacity.is_acquired
;;

let cleanup_temporary t entry state ~event =
  let instance = state.Agent_session.Session_state.spec.workspace_instance in
  match instance.source_kind with
  | Physical | Current -> Ok instance
  | Temporary _ ->
    let open Result.Let_syntax in
    let%bind _handle, protected_roots, expected_path = cleanup_context t entry instance in
    Agent_session.Workspace_cleanup.cleanup
      ~env:t.env
      ~protected_roots
      ~expected_path
      ~has_active_lease:(has_active_workspace_lease entry)
      ~event
      ~now:(now t)
      instance
    |> Result.map_error ~f:persistence_error
;;

let remove_temporary t entry state =
  let instance = state.Agent_session.Session_state.spec.workspace_instance in
  let open Result.Let_syntax in
  let%bind _handle, protected_roots, expected_path = cleanup_context t entry instance in
  Agent_session.Workspace_cleanup.remove
    ~env:t.env
    ~protected_roots
    ~expected_path
    ~has_active_lease:(has_active_workspace_lease entry)
    ~now:(now t)
    instance
  |> Result.map_error ~f:persistence_error
;;

let workspace_definition t instance =
  let open Result.Let_syntax in
  let%bind definition_id =
    instance.Agent_session.Workspace_instance.definition_id
    |> Result.of_option ~error:(error Invalid_state "workspace has no definition")
  in
  Agent_session.Workspace_catalog.find t.workspaces definition_id
  |> Result.of_option ~error:(error Workspace_not_found "workspace is not in the catalog")
;;

let resolve_replacement t entry instance =
  let open Result.Let_syntax in
  let%bind handle =
    entry.Session_registry.store_handle
    |> Result.of_option ~error:(error Invalid_state "session has no durable workspace")
  in
  let%bind definition = workspace_definition t instance in
  Agent_session.Workspace_resolver.resolve
    ~env:t.env
    ~instance_id:(Agent_protocol.Id.Workspace_instance.create ())
    ~session_directory:(Agent_store.Session_store.Handle.directory handle)
    definition
  |> Result.map_error ~f:persistence_error
;;

let update_workspace_capacity entry instance =
  match entry.Session_registry.capacity with
  | None -> Ok ()
  | Some capacity ->
    Session_capacity.replace_workspace
      capacity
      ~conflict_domain:instance.Agent_session.Workspace_instance.conflict_domain
      ~workspace_lease_mode:(workspace_lease_mode instance.access)
    |> Result.map_error ~f:(error Invalid_state)
;;

let install_replacement entry instance =
  let open Result.Let_syntax in
  let%bind session =
    Agent_session.Session_actor.replace_workspace entry.Session_registry.actor instance
  in
  let%map () = update_workspace_capacity entry instance in
  session
;;

let replacement_temporary t entry state =
  let open Result.Let_syntax in
  let%bind removed = remove_temporary t entry state in
  resolve_replacement t entry removed
;;

let cleanup_stopped_workspace t entry =
  let open Result.Let_syntax in
  let%bind state = Agent_session.Session_actor.state entry.Session_registry.actor in
  let original = state.spec.workspace_instance in
  let%bind cleaned =
    cleanup_temporary t entry state ~event:Agent_session.Workspace_cleanup.Session_stop
  in
  if Option.is_none cleaned.cleanup_completion
  then Ok (Agent_session.Session_state.summary state)
  else (
    let%bind replacement = resolve_replacement t entry original in
    install_replacement entry replacement)
;;

type idempotency =
  { session_id : Agent_protocol.Id.Session.t option
  ; key : Agent_protocol.Idempotency_key.t
  ; retention : Agent_store.Idempotency_store.retention
  }

let standard session_id key =
  Some { session_id; key; retention = Agent_store.Idempotency_store.Standard }
;;

let protected session_id key =
  Some { session_id; key; retention = Agent_store.Idempotency_store.Protected }
;;

let idempotency = function
  | Agent_protocol.Command.Session_create request ->
    standard None request.Agent_protocol.Session.Create_request.idempotency_key
  | Session_attach request -> standard (Some request.session_id) request.idempotency_key
  | Session_detach request -> standard (Some request.session_id) request.idempotency_key
  | Session_renew_owner request ->
    standard (Some request.session_id) request.idempotency_key
  | Session_start request -> standard (Some request.session_id) request.idempotency_key
  | Session_stop request -> standard (Some request.session_id) request.idempotency_key
  | Session_cancel_operation request ->
    standard (Some request.session_id) request.idempotency_key
  | Session_send_message request ->
    protected (Some request.session_id) request.idempotency_key
  | Session_compact request -> protected (Some request.session_id) request.idempotency_key
  | Session_delete_history request ->
    protected (Some request.session_id) request.idempotency_key
  | Session_reset request -> protected (Some request.session_id) request.idempotency_key
  | Session_rebuild request -> protected (Some request.session_id) request.idempotency_key
  | Session_upgrade_prompt request ->
    protected (Some request.session_id) request.idempotency_key
  | Session_delete request -> protected (Some request.session_id) request.idempotency_key
  | Permission_respond request ->
    standard (Some request.session_id) request.idempotency_key
  | Grant_revoke request -> standard (Some request.session_id) request.idempotency_key
  | Job_cancel request -> protected (Some request.session_id) request.idempotency_key
  | Schedule_create request -> protected (Some request.session_id) request.idempotency_key
  | Schedule_cancel request -> protected (Some request.session_id) request.idempotency_key
  | Protocol_initialize _
  | Protocol_ping _
  | Server_info
  | Server_health _
  | Prompt_list _
  | Prompt_get _
  | Workspace_list _
  | Workspace_get _
  | Blob_read _
  | Session_list _
  | Session_get _
  | Session_export _
  | Permission_list _
  | Grant_list _
  | Audit_read _
  | Job_list _
  | Job_get _
  | Schedule_list _
  | Schedule_get _ -> None
;;

let request_digest command =
  Agent_protocol.Command.params command
  |> Agent_protocol.Json_codec.canonical_string
  |> Result.map ~f:(fun encoded ->
    Digestif.SHA256.digest_string encoded |> Digestif.SHA256.to_hex)
;;

let expiration now =
  Agent_protocol.Timestamp.to_time_ns now
  |> Fn.flip Time_ns.add (Time_ns.Span.of_day 1.)
  |> Agent_protocol.Timestamp.of_time_ns
;;

let idempotency_key principal command (identity : idempotency) =
  Agent_store.Idempotency_store.Key.
    { principal_id = principal.Agent_protocol.Principal.id
    ; session_id = identity.session_id
    ; method_name = Agent_protocol.Command.method_name command
    ; idempotency_key = identity.key
    }
;;

let replay_outcome command = function
  | Agent_store.Idempotency_store.Pending ->
    Error
      (Agent_protocol.Error.create
         Interrupted
         ~message:
           "the original command outcome is unknown; duplicate execution is suppressed"
         ~retryable:false
         ())
  | Agent_store.Idempotency_store.Success json ->
    Agent_protocol.Method_result.of_json
      ~method_:(Agent_protocol.Command.method_name command)
      json
  | Failure error -> Error error
;;

let audit_read_error = function
  | Agent_store.Store_error.Corrupt message
    when String.is_prefix message ~prefix:"audit cursor" ->
    Agent_protocol.Error.invalid_request message
  | failure -> persistence_error failure
;;

let pending_record t key digest retention =
  let now = now t in
  Agent_store.Idempotency_store.record
    t.idempotency_store
    { key
    ; request_digest = digest
    ; accepted_transaction_sequence = None
    ; outcome = Pending
    ; created_at = now
    ; expires_at = Some (expiration now)
    ; retention
    }
  |> Result.map_error ~f:persistence_error
;;

let store_outcome t key digest result =
  let outcome =
    match result with
    | Ok value ->
      Agent_store.Idempotency_store.Success (Agent_protocol.Method_result.to_json value)
    | Error failure -> Failure failure
  in
  Agent_store.Idempotency_store.complete
    t.idempotency_store
    ~key
    ~request_digest:digest
    ~accepted_transaction_sequence:None
    ~outcome
  |> Result.map_error ~f:persistence_error
  |> Result.bind ~f:(fun _ -> result)
;;

let handle_idempotent t context command identity execute =
  Eio.Mutex.use_rw ~protect:true t.idempotency_mutex (fun () ->
    let open Result.Let_syntax in
    let principal = Connection_context.principal context in
    let key = idempotency_key principal command identity in
    let%bind digest = request_digest command in
    match
      Agent_store.Idempotency_store.lookup t.idempotency_store ~key ~request_digest:digest
    with
    | Replay record -> replay_outcome command record.outcome
    | Conflict _ ->
      Error (error Idempotency_conflict "idempotency key was used for another request")
    | Missing ->
      let%bind _ = pending_record t key digest identity.retention in
      let command_audit =
        Agent_store.Idempotency_store.Command_audit.
          { key
          ; request_digest = digest
          ; protected_record =
              (match identity.retention with
               | Standard -> false
               | Protected -> true)
          }
        |> Agent_store.Idempotency_store.Command_audit.encode
      in
      store_outcome t key digest (execute (Some command_audit)))
;;

let mutation session =
  Agent_protocol.Mutation_result.
    { revision = session.Agent_protocol.Session.revision
    ; latest_event_sequence = session.latest_event_sequence
    }
;;

let session_mutation session =
  Agent_protocol.Method_result.Session_mutation.{ session; mutation = mutation session }
;;

let actor_command command_audit ~plain ~audited =
  match command_audit with
  | None -> plain ()
  | Some command_audit -> audited command_audit
;;

let find_entry t session_id = Session_registry.load t.registry session_id

let principal_is_admin principal =
  Agent_protocol.Principal.has_scope principal Administer_configuration
;;

let session_visible_to principal (session : Agent_protocol.Session.t) =
  principal_is_admin principal
  || Option.value_map
       session.Agent_protocol.Session.creator
       ~default:false
       ~f:(fun creator ->
         Agent_protocol.Id.Principal.compare creator principal.Agent_protocol.Principal.id
         = 0)
;;

let state_visible_to principal state =
  session_visible_to principal (Agent_session.Session_state.summary state)
;;

let find_visible_entry t context session_id =
  let open Result.Let_syntax in
  let%bind entry = find_entry t session_id in
  let%bind state = Agent_session.Session_actor.state entry.actor in
  if state_visible_to (Connection_context.principal context) state
  then Ok (entry, state)
  else Error (error Permission_denied "session is not visible to this principal")
;;

let require_connection_attachment context ~session_id ~attachment_id =
  if Connection_context.owns_attachment context ~session_id ~attachment_id
  then Ok ()
  else Error (error Permission_denied "attachment is not owned by this connection")
;;

let require_connection_writer context attachment_id =
  Connection_context.attachments context
  |> List.find ~f:(fun (attachment : Agent_protocol.Session.Attachment.t) ->
    Agent_protocol.Id.Attachment.compare attachment.id attachment_id = 0)
  |> function
  | Some (attachment : Agent_protocol.Session.Attachment.t) ->
    (match attachment.mode with
     | Agent_protocol.Session.Read_only ->
       Error (error Permission_denied "read-only attachment cannot mutate the session")
     | Read_write | Owner_read_write -> Ok ())
  | None -> Error (error Permission_denied "attachment is not owned by this connection")
;;

let page _limit items = Agent_protocol.Page.{ items; next_cursor = None }

let prompt_available = function
  | Agent_session.Prompt_catalog.Ready _ -> true
  | Unavailable _ | Disabled -> false
;;

let handle_prompt_list t request =
  let items =
    Agent_session.Prompt_catalog.entries t.prompts
    |> List.filter ~f:(fun entry ->
      Option.value_map
        request.Agent_protocol.Prompt.List_request.enabled
        ~default:true
        ~f:(Bool.equal entry.definition.enabled)
      && Option.value_map
           request.available
           ~default:true
           ~f:(Bool.equal (prompt_available entry.availability)))
    |> List.map ~f:Catalog_projection.prompt
  in
  Ok (Agent_protocol.Method_result.Prompt_list (page request.page.limit items))
;;

let handle_prompt_get t request =
  Agent_session.Prompt_catalog.find
    t.prompts
    request.Agent_protocol.Prompt.Get_request.prompt_id
  |> Option.map ~f:Catalog_projection.prompt
  |> Result.of_option ~error:(error Prompt_not_found "prompt is not in the catalog")
  |> Result.map ~f:(fun prompt -> Agent_protocol.Method_result.Prompt_get prompt)
;;

let workspace_matches request definition =
  let projection = Catalog_projection.workspace definition in
  Option.value_map
    request.Agent_protocol.Workspace.List_request.kind
    ~default:true
    ~f:(fun kind -> Agent_protocol.Workspace.equal_kind kind projection.kind)
  && Option.value_map request.access ~default:true ~f:(fun access ->
    Agent_protocol.Workspace.equal_access access projection.access)
  && Option.value_map request.available ~default:true ~f:Fn.id
;;

let handle_workspace_list t request =
  let items =
    Agent_session.Workspace_catalog.definitions t.workspaces
    |> List.filter ~f:(workspace_matches request)
    |> List.map ~f:Catalog_projection.workspace
  in
  Ok (Agent_protocol.Method_result.Workspace_list (page request.page.limit items))
;;

let handle_workspace_get t request =
  Agent_session.Workspace_catalog.find
    t.workspaces
    request.Agent_protocol.Workspace.Get_request.workspace_id
  |> Option.map ~f:Catalog_projection.workspace
  |> Result.of_option ~error:(error Workspace_not_found "workspace is not in the catalog")
  |> Result.map ~f:(fun workspace -> Agent_protocol.Method_result.Workspace_get workspace)
;;

let handle_blob_read t context request =
  let open Result.Let_syntax in
  let%bind () =
    require_connection_attachment
      context
      ~session_id:request.Agent_protocol.Blob.Read_request.session_id
      ~attachment_id:request.attachment_id
  in
  let%bind entry, _ = find_visible_entry t context request.session_id in
  let%bind store_handle =
    entry.Session_registry.store_handle
    |> Result.of_option ~error:(error Invalid_state "session has no durable blob store")
  in
  let%bind handle =
    Agent_store.Blob_store.open_session t.blob_store store_handle request.blob_id
    |> Result.map_error ~f:persistence_error
  in
  let blob = (Agent_store.Blob_store.Handle.metadata handle).blob in
  let%bind () =
    if
      Principal_projection.can_read_blob
        (Connection_context.principal context)
        (Agent_store.Blob_store.Handle.metadata handle)
    then Ok ()
    else Error (error Permission_denied "blob requires additional principal scopes")
  in
  if Int64.(request.offset > blob.byte_length)
  then Error (error Invalid_request "blob read offset exceeds the blob length")
  else (
    let%map data =
      Agent_store.Blob_store.read_range
        t.blob_store
        ~sw:t.sw
        handle
        ~offset:request.offset
        ~max_bytes:request.max_bytes
      |> Result.map_error ~f:persistence_error
    in
    let next_offset = Int64.(request.offset + of_int (String.length data)) in
    Agent_protocol.Method_result.Blob_read
      { blob
      ; offset = request.offset
      ; next_offset
      ; data_base64 = Base64.encode_exn data
      ; eof = Int64.equal next_offset blob.byte_length
      })
;;

let rec forward_subscriber context subscriber =
  match Agent_session.Subscriber.take subscriber with
  | None -> ()
  | Some (Error _) -> ()
  | Some (Ok (Durable event)) ->
    Connection_context.publish_notification
      context
      (Principal_projection.durable (Connection_context.principal context) event
       |> Agent_protocol.Event.Durable.to_notification);
    forward_subscriber context subscriber
  | Some (Ok (Recoverable event)) ->
    Option.iter
      (Principal_projection.recoverable (Connection_context.principal context) event)
      ~f:(fun event ->
        Connection_context.publish_notification
          context
          (Agent_protocol.Event.Recoverable.to_notification event));
    forward_subscriber context subscriber
;;

let authorize_attachment_mode principal mode =
  if not (Agent_protocol.Principal.has_scope principal View_session_transcript)
  then Error (error Permission_denied "attachment requires transcript scope")
  else (
    match mode with
    | Agent_protocol.Session.Read_only -> Ok ()
    | Read_write ->
      if Agent_protocol.Principal.has_scope principal Send_messages
      then Ok ()
      else Error (error Permission_denied "read/write attachment requires send_messages")
    | Owner_read_write ->
      if
        Agent_protocol.Principal.has_scope principal Send_messages
        && Agent_protocol.Principal.has_scope principal Own_sessions
      then Ok ()
      else
        Error
          (error
             Permission_denied
             "owner attachment requires send_messages and own_sessions"))
;;

let attach_entry
      t
      context
      entry
      ~command_audit
      ~mode
      ~subscribe
      ~after_sequence
      ~reclaim_token
  =
  let open Result.Let_syntax in
  let%bind () = Connection_context.reserve_attachment context in
  let principal_id = (Connection_context.principal context).id in
  let attached =
    actor_command
      command_audit
      ~plain:(fun () ->
        Agent_session.Session_actor.attach_with_snapshot
          entry.Session_registry.actor
          ~principal_id:(Some principal_id)
          ~reclaim_token
          ~mode
          ~subscribe)
      ~audited:(fun command_audit ->
        Agent_session.Session_actor.attach_with_snapshot_and_command_audit
          entry.actor
          ~command_audit
          ~principal_id:(Some principal_id)
          ~reclaim_token
          ~mode
          ~subscribe)
  in
  match attached with
  | Error _ as failure ->
    Connection_context.release_attachment_reservation context;
    failure
  | Ok (attachment, subscriber, snapshot, issued_reclaim_token) ->
    let principal = Connection_context.principal context in
    let snapshot = Principal_projection.snapshot principal snapshot in
    Connection_context.register_reserved_attachment context attachment;
    Option.iter subscriber ~f:(fun subscriber ->
      Eio.Fiber.fork ~sw:t.sw (fun () -> forward_subscriber context subscriber));
    let replay =
      match after_sequence with
      | None -> Agent_protocol.Method_result.Attach.Snapshot snapshot
      | Some after_sequence ->
        (match
           Agent_session.Durable_event_log.replay
             entry.durable_events
             ~after_sequence
             ~through_sequence:snapshot.latest_event_sequence
         with
         | Available events ->
           Events (List.map events ~f:(Principal_projection.durable principal))
         | Snapshot_required -> Snapshot snapshot)
    in
    Ok
      Agent_protocol.Method_result.Attach.
        { attachment
        ; replay
        ; latest_event_sequence = snapshot.latest_event_sequence
        ; reclaim_token = issued_reclaim_token
        }
;;

let handle_session_create t context command_audit request =
  let open Result.Let_syntax in
  let%bind () =
    match
      request.Agent_protocol.Session.Create_request.requested_mode, request.subscribe
    with
    | None, true ->
      Error (error Invalid_request "subscribe requires a requested attachment mode")
    | None, false -> Ok ()
    | Some mode, _ ->
      authorize_attachment_mode (Connection_context.principal context) mode
  in
  let%bind entry =
    t.create_session
      ~command_audit
      ~principal:(Connection_context.principal context)
      request
  in
  let%bind state = Agent_session.Session_actor.state entry.actor in
  let%bind () =
    Session_registry.add t.registry ~session_id:state.identity.session_id entry
  in
  let%bind attachment =
    match request.requested_mode with
    | None -> Ok None
    | Some mode ->
      Result.map
        (attach_entry
           t
           context
           entry
           ~command_audit:None
           ~mode
           ~subscribe:request.subscribe
           ~after_sequence:None
           ~reclaim_token:None)
        ~f:Option.some
  in
  let%map state = Agent_session.Session_actor.state entry.actor in
  let session = Agent_session.Session_state.summary state in
  Agent_protocol.Method_result.Session_create
    { session; mutation = mutation session; attachment }
;;

let labels_match requested actual =
  List.for_all requested ~f:(fun (name, value) ->
    List.Assoc.find actual name ~equal:String.equal
    |> Option.exists ~f:(String.equal value))
;;

let session_matches request (summary : Agent_protocol.Session.t) =
  Option.value_map
    request.Agent_protocol.Session.List_request.desired_state
    ~default:true
    ~f:(fun desired ->
      Agent_protocol.Session.equal_desired_state desired summary.desired_state)
  && Option.value_map request.prompt_id ~default:true ~f:(fun prompt_id ->
    match summary.spec.prompt with
    | Catalog actual -> Agent_protocol.Id.Prompt_definition.compare prompt_id actual = 0
    | Local_path _ -> false)
  && Option.value_map request.workspace_id ~default:true ~f:(fun workspace_id ->
    match summary.spec.workspace with
    | Configured actual ->
      Agent_protocol.Id.Workspace_definition.compare workspace_id actual = 0
    | Current | Local_path _ -> false)
  && Option.value_map request.owner_principal_id ~default:true ~f:(fun owner ->
    Option.exists summary.creator ~f:(fun actual ->
      Agent_protocol.Id.Principal.compare owner actual = 0))
  && labels_match request.labels summary.spec.labels
;;

let handle_session_list t context request =
  let principal = Connection_context.principal context in
  let sessions =
    Session_registry.summaries t.registry
    |> List.filter ~f:(session_visible_to principal)
    |> List.filter ~f:(session_matches request)
  in
  Ok (Agent_protocol.Method_result.Session_list (page request.page.limit sessions))
;;

let handle_session_get t context request =
  let open Result.Let_syntax in
  let%bind entry, _ =
    find_visible_entry t context request.Agent_protocol.Session.Get_request.session_id
  in
  let%bind snapshot = Agent_session.Session_actor.snapshot entry.actor in
  let%map snapshot =
    Pagination.history
      t.pagination
      (Connection_context.principal context)
      request
      snapshot
  in
  Agent_protocol.Method_result.Session_get
    (Principal_projection.snapshot (Connection_context.principal context) snapshot)
;;

let handle_session_attach t context command_audit request =
  let open Result.Let_syntax in
  let%bind () =
    authorize_attachment_mode
      (Connection_context.principal context)
      request.Agent_protocol.Session.Attach_request.requested_mode
  in
  let%bind entry, _ =
    find_visible_entry t context request.Agent_protocol.Session.Attach_request.session_id
  in
  let%map attachment =
    attach_entry
      t
      context
      entry
      ~command_audit
      ~mode:request.requested_mode
      ~subscribe:request.subscribe
      ~after_sequence:request.after_sequence
      ~reclaim_token:request.reclaim_token
  in
  Agent_protocol.Method_result.Session_attach attachment
;;

let handle_session_detach t context command_audit request =
  let open Result.Let_syntax in
  let%bind entry =
    find_entry t request.Agent_protocol.Session.Detach_request.session_id
  in
  let%bind () =
    require_connection_attachment
      context
      ~session_id:request.session_id
      ~attachment_id:request.attachment_id
  in
  let%bind () =
    actor_command
      command_audit
      ~plain:(fun () ->
        Agent_session.Session_actor.detach entry.actor request.attachment_id)
      ~audited:(fun command_audit ->
        Agent_session.Session_actor.detach_with_command_audit
          entry.actor
          ~command_audit
          request.attachment_id)
  in
  Connection_context.remove_attachment context request.attachment_id;
  let%map state = Agent_session.Session_actor.state entry.actor in
  Agent_protocol.Method_result.Session_detach
    (mutation (Agent_session.Session_state.summary state))
;;

let handle_session_renew_owner t context command_audit request =
  let open Result.Let_syntax in
  let%bind () =
    require_connection_attachment
      context
      ~session_id:request.Agent_protocol.Session.Renew_owner_request.session_id
      ~attachment_id:request.attachment_id
  in
  let%bind entry = find_entry t request.session_id in
  let%map lease, session =
    actor_command
      command_audit
      ~plain:(fun () ->
        Agent_session.Session_actor.renew_owner
          entry.actor
          ~attachment_id:request.attachment_id
          ~lease_generation:request.lease_generation)
      ~audited:(fun command_audit ->
        Agent_session.Session_actor.renew_owner_with_command_audit
          entry.actor
          ~command_audit
          ~attachment_id:request.attachment_id
          ~lease_generation:request.lease_generation)
  in
  Agent_protocol.Method_result.Session_renew_owner (lease, mutation session)
;;

let with_writer t context ~session_id ~attachment_id f =
  let open Result.Let_syntax in
  let%bind () = require_connection_attachment context ~session_id ~attachment_id in
  let%bind entry = find_entry t session_id in
  let%bind () = Agent_session.Session_actor.authorize_writer entry.actor ~attachment_id in
  f entry
;;

let rec handle_session_start t context command_audit request =
  with_writer
    t
    context
    ~session_id:request.Agent_protocol.Session.Start_request.session_id
    ~attachment_id:request.attachment_id
    (fun entry ->
       let open Result.Let_syntax in
       let%bind state = Agent_session.Session_actor.state entry.actor in
       let%bind () =
         Agent_session.Workspace_resolver.verify_available
           ~env:t.env
           state.spec.workspace_instance
         |> Result.map_error ~f:workspace_unavailable
       in
       match state.lifecycle.observed, entry.capacity with
       | Queued_for_slot, _ ->
         Ok
           (Agent_protocol.Method_result.Session_start
              (session_mutation (Agent_session.Session_state.summary state)))
       | _, None ->
         let%bind () = Runtime_owner.ensure_loaded entry.runtime in
         actor_command
           command_audit
           ~plain:(fun () ->
             Agent_session.Session_actor.start
               entry.actor
               ~attachment_id:request.attachment_id)
           ~audited:(fun command_audit ->
             Agent_session.Session_actor.start_with_command_audit
               entry.actor
               ~command_audit
               ~attachment_id:request.attachment_id)
         |> Result.map ~f:(fun session ->
           Agent_protocol.Method_result.Session_start (session_mutation session))
       | _, Some capacity -> handle_capacity_start t entry capacity command_audit request)

and enqueue_start t state =
  let open Result.Let_syntax in
  let%bind quota_key =
    state.Agent_session.Session_state.spec.quota_key
    |> Result.of_option ~error:(error Invalid_state "session has no quota key")
  in
  Agent_session.Start_queue.enqueue
    t.start_queue
    { session_id = state.identity.session_id
    ; accepted_command_sequence = state.counters.revision
    ; quota_key
    ; created_at = now t
    }

and queue_capacity_start t entry command_audit request =
  let open Result.Let_syntax in
  let%bind session =
    actor_command
      command_audit
      ~plain:(fun () ->
        Agent_session.Session_actor.queue_start
          entry.Session_registry.actor
          ~attachment_id:request.Agent_protocol.Session.Start_request.attachment_id)
      ~audited:(fun command_audit ->
        Agent_session.Session_actor.queue_start_with_command_audit
          entry.actor
          ~command_audit
          ~attachment_id:request.attachment_id)
  in
  let%bind state = Agent_session.Session_actor.state entry.actor in
  let%map () = enqueue_start t state in
  Agent_protocol.Method_result.Session_start (session_mutation session)

and acquired_capacity_start entry capacity command_audit request ~newly_acquired =
  let started =
    let open Result.Let_syntax in
    let%bind () = Runtime_owner.ensure_loaded entry.Session_registry.runtime in
    actor_command
      command_audit
      ~plain:(fun () ->
        Agent_session.Session_actor.start
          entry.actor
          ~attachment_id:request.Agent_protocol.Session.Start_request.attachment_id)
      ~audited:(fun command_audit ->
        Agent_session.Session_actor.start_with_command_audit
          entry.actor
          ~command_audit
          ~attachment_id:request.attachment_id)
  in
  match started with
  | Error _ as failure ->
    if newly_acquired then Session_capacity.release capacity;
    failure
  | Ok session ->
    Session_capacity.runtime_ready capacity;
    Ok (Agent_protocol.Method_result.Session_start (session_mutation session))

and handle_capacity_start t entry capacity command_audit request =
  match
    Session_capacity.try_acquire
      capacity
      ~queue_if_limited:request.Agent_protocol.Session.Start_request.queue_if_limited
  with
  | Acquired ->
    acquired_capacity_start entry capacity command_audit request ~newly_acquired:true
  | Already_acquired ->
    acquired_capacity_start entry capacity command_audit request ~newly_acquired:false
  | Queue_required _ -> queue_capacity_start t entry command_audit request
  | Rejected error -> Error error
;;

let handle_session_stop t context command_audit request =
  with_writer
    t
    context
    ~session_id:request.Agent_protocol.Session.Stop_request.session_id
    ~attachment_id:request.attachment_id
    (fun entry ->
       ignore (Agent_session.Start_queue.cancel t.start_queue request.session_id : bool);
       let open Result.Let_syntax in
       let%bind session =
         actor_command
           command_audit
           ~plain:(fun () ->
             Agent_session.Session_actor.stop
               entry.actor
               ~attachment_id:request.attachment_id
               ~mode:request.mode)
           ~audited:(fun command_audit ->
             Agent_session.Session_actor.stop_with_command_audit
               entry.actor
               ~command_audit
               ~attachment_id:request.attachment_id
               ~mode:request.mode)
       in
       let%bind () =
         match session.observed_state with
         | Agent_protocol.Session.Stopped -> Runtime_owner.unload_and_wait entry.runtime
         | Queued_for_slot
         | Starting
         | Recovering
         | Idle
         | Running_turn _
         | Compacting _
         | Waiting_for_permission _
         | Stopping
         | Failed _ -> Ok ()
       in
       let%map session = cleanup_stopped_workspace t entry in
       Agent_protocol.Method_result.Session_stop (session_mutation session))
;;

let handle_session_cancel_operation t context command_audit request =
  with_writer
    t
    context
    ~session_id:request.Agent_protocol.Session.Cancel_operation_request.session_id
    ~attachment_id:request.attachment_id
    (fun entry ->
       actor_command
         command_audit
         ~plain:(fun () ->
           Agent_session.Session_actor.cancel_operation
             entry.actor
             ~attachment_id:request.attachment_id
             ~operation_id:request.operation_id)
         ~audited:(fun command_audit ->
           Agent_session.Session_actor.cancel_operation_with_command_audit
             entry.actor
             ~command_audit
             ~attachment_id:request.attachment_id
             ~operation_id:request.operation_id)
       |> Result.map ~f:(fun session ->
         Agent_protocol.Method_result.Session_cancel_operation (session_mutation session)))
;;

let handle_delete_history t context command_audit request =
  with_writer
    t
    context
    ~session_id:request.Agent_protocol.Session.Delete_history_request.session_id
    ~attachment_id:request.attachment_id
    (fun entry ->
       Agent_session.Session_actor.delete_history
         entry.actor
         ?command_audit
         ~attachment_id:request.attachment_id
         ~expected_revision:request.expected_revision
         request.history_id
       |> Result.map ~f:(fun session ->
         Agent_protocol.Method_result.Session_delete_history (session_mutation session)))
;;

let handle_session_compact t context command_audit request =
  with_writer
    t
    context
    ~session_id:request.Agent_protocol.Session.Compact_request.session_id
    ~attachment_id:request.attachment_id
    (fun entry ->
       actor_command
         command_audit
         ~plain:(fun () ->
           Agent_session.Session_actor.compact
             entry.actor
             ~attachment_id:request.attachment_id
             ~expected_revision:request.expected_revision)
         ~audited:(fun command_audit ->
           Agent_session.Session_actor.compact_with_command_audit
             entry.actor
             ~command_audit
             ~attachment_id:request.attachment_id
             ~expected_revision:request.expected_revision)
       |> Result.map ~f:(fun session ->
         Agent_protocol.Method_result.Session_compact (session_mutation session)))
;;

let handle_send_message t context command_audit request =
  let open Result.Let_syntax in
  let content = request.Agent_protocol.Session.Send_message_request.content in
  let%bind () =
    if String.is_empty (String.strip content.text)
    then Error (error Invalid_request "message text must be nonempty")
    else Ok ()
  in
  with_writer
    t
    context
    ~session_id:request.session_id
    ~attachment_id:request.attachment_id
    (fun entry ->
       let%bind () = require_connection_writer context request.attachment_id in
       let%bind history_id = Agent_session.History_id_source.allocate entry.history_ids in
       let%bind history_entry =
         Runtime_owner.parse_user_content entry.runtime ~id:history_id content
       in
       let history_entry = Agent_session.History_codec.to_protocol history_entry in
       let%bind submission =
         actor_command
           command_audit
           ~plain:(fun () ->
             Agent_session.Session_actor.submit_message
               entry.actor
               ~attachment_id:request.attachment_id
               history_entry)
           ~audited:(fun command_audit ->
             Agent_session.Session_actor.submit_message_with_command_audit
               entry.actor
               ~command_audit
               ~attachment_id:request.attachment_id
               history_entry)
       in
       Ok
         (Agent_protocol.Method_result.Session_send_message
            { history_id = submission.history_id
            ; disposition = submission.disposition
            ; operation_id = submission.operation_id
            ; mutation = mutation submission.session
            }))
;;

let render_export request snapshot entries =
  match request.Agent_protocol.Session.Export_request.format with
  | Json ->
    Ok
      ( "application/json"
      , "session.json"
      , `Object
          [ "snapshot", Agent_protocol.Snapshot.to_json snapshot
          ; "history", `Array (List.map entries ~f:Agent_protocol.History.entry_to_json)
          ]
        |> Jsonaf.to_string )
  | Chatmd ->
    let entries =
      List.map entries ~f:(fun (entry : Agent_protocol.History.entry) ->
        if not entry.redacted
        then entry
        else
          { entry with
            role = Assistant
          ; kind = Message
          ; redacted = false
          ; payload =
              `Object
                [ "type", `String "message"
                ; "role", `String "assistant"
                ; ( "content"
                  , `Array
                      [ `Object
                          [ "type", `String "input_text"
                          ; "text", `String "[Tool content redacted]"
                          ]
                      ] )
                ]
          })
    in
    Agent_session.History_codec.all_of_protocol entries
    |> Result.map ~f:(fun history ->
      ( "text/markdown; charset=utf-8"
      , "session.chatmd"
      , Agent_session.Chatmd_export.render history ))
;;

let create_export_blob
      t
      principal
      session_id
      store_handle
      ~media_type
      ~display_name
      content
  =
  let open Result.Let_syntax in
  let id = Agent_protocol.Id.Blob.create () in
  let%bind upload =
    Agent_store.Blob_store.begin_upload
      t.blob_store
      ~sw:t.sw
      ~id
      ~creating_principal:principal.Agent_protocol.Principal.id
      ~target_session:(Some session_id)
      ~kind:File
      ~media_type
      ~display_name:(Some display_name)
      ~allowed_use:(Principal_projection.export_use principal)
      ~created_at:(now t)
      ~expires_at:None
    |> Result.map_error ~f:persistence_error
  in
  let%bind () =
    Agent_store.Blob_store.write_string upload content
    |> Result.map_error ~f:persistence_error
  in
  let%bind handle =
    Agent_store.Blob_store.finish upload ~expected_digest:None
    |> Result.map_error ~f:persistence_error
  in
  Agent_store.Blob_store.adopt t.blob_store store_handle handle
  |> Result.map_error ~f:persistence_error
;;

let export_snapshot t entry revision =
  let open Result.Let_syntax in
  let%bind state = Agent_session.Session_actor.state entry.Session_registry.actor in
  match revision with
  | None -> Agent_session.Session_actor.snapshot entry.actor
  | Some revision when Int64.equal revision state.counters.revision ->
    Ok (Agent_session.Session_state.snapshot ~now:state.identity.updated_at state)
  | Some revision ->
    let%bind reference =
      List.find state.conversation.compaction_archives ~f:(fun archive ->
        Int64.equal archive.revision revision)
      |> Result.of_option
           ~error:(error Conflict "requested revision has no retained archive")
    in
    let%bind handle =
      entry.store_handle
      |> Result.of_option
           ~error:(error Invalid_state "session has no durable archive store")
    in
    let%map archived =
      Agent_session.Compaction_archive.read
        ~env:t.env
        ~handle
        ~max_payload_length:Int.max_value
        reference
    in
    Agent_session.Session_state.snapshot ~now:archived.identity.updated_at archived
;;

let handle_session_export t context request =
  let open Result.Let_syntax in
  let%bind () =
    require_connection_attachment
      context
      ~session_id:request.Agent_protocol.Session.Export_request.session_id
      ~attachment_id:request.attachment_id
  in
  let%bind entry, _ = find_visible_entry t context request.session_id in
  let%bind snapshot = export_snapshot t entry request.revision in
  let%bind snapshot =
    Pagination.history
      t.pagination
      (Connection_context.principal context)
      { session_id = request.session_id; history = request.history }
      snapshot
  in
  let snapshot =
    Principal_projection.snapshot (Connection_context.principal context) snapshot
  in
  let entries =
    if Option.exists request.history ~f:(fun history -> history.effective)
    then (Option.value_exn snapshot.effective_history).entries
    else snapshot.canonical_history.entries
  in
  let%bind media_type, display_name, content = render_export request snapshot entries in
  let%bind store_handle =
    entry.store_handle
    |> Result.of_option ~error:(error Invalid_state "session has no durable blob store")
  in
  let%map handle =
    create_export_blob
      t
      (Connection_context.principal context)
      request.session_id
      store_handle
      ~media_type
      ~display_name
      content
  in
  Agent_protocol.Method_result.Session_export
    { blob = (Agent_store.Blob_store.Handle.metadata handle).blob
    ; session_revision = snapshot.revision
    ; latest_event_sequence = snapshot.latest_event_sequence
    }
;;

let validate_delete_state request state =
  if
    not
      (Int64.equal
         request.Agent_protocol.Session.Delete_request.expected_revision
         state.Agent_session.Session_state.counters.revision)
  then Error (error Conflict "session revision does not match")
  else if Option.is_some state.active_operation
  then Error (error Conflict "session has an active foreground operation")
  else if
    match state.lifecycle.observed with
    | Agent_protocol.Session.Stopped -> false
    | Queued_for_slot
    | Starting
    | Recovering
    | Idle
    | Running_turn _
    | Compacting _
    | Waiting_for_permission _
    | Stopping
    | Failed _ -> true
  then Error (error Invalid_state "session must be stopped before deletion")
  else if
    not
      (String.equal
         request.confirmation
         (Agent_protocol.Id.Session.to_string request.session_id))
  then Error (error Invalid_request "deletion confirmation must equal the session ID")
  else Ok ()
;;

let close_deleted_entry t context request entry =
  ignore
    (Session_registry.remove
       t.registry
       request.Agent_protocol.Session.Delete_request.session_id
     : Session_registry.entry option);
  Connection_context.remove_session_attachments context request.session_id;
  entry.Session_registry.close ()
;;

let handle_session_delete t context request =
  let open Result.Let_syntax in
  let%bind () =
    require_connection_attachment
      context
      ~session_id:request.Agent_protocol.Session.Delete_request.session_id
      ~attachment_id:request.attachment_id
  in
  let%bind entry, state = find_visible_entry t context request.session_id in
  let%bind () = validate_delete_state request state in
  let%bind (_ : Agent_session.Workspace_instance.t) =
    cleanup_temporary t entry state ~event:Agent_session.Workspace_cleanup.Session_delete
  in
  let deleted_at = now t in
  let%bind () =
    match request.policy with
    | Agent_protocol.Session.Delete_request.Archive ->
      let%map () =
        Agent_store.Session_store.archive_session t.session_store request.session_id
        |> Result.map_error ~f:persistence_error
      in
      close_deleted_entry t context request entry
    | Remove ->
      close_deleted_entry t context request entry;
      Agent_store.Session_store.remove_session t.session_store request.session_id
      |> Result.map_error ~f:persistence_error
  in
  Ok
    (Agent_protocol.Method_result.Session_delete
       { session_id = request.session_id; deleted_at; archive = None })
;;

let validate_stopped_revision state expected_revision =
  if
    not
      (Int64.equal expected_revision state.Agent_session.Session_state.counters.revision)
  then Error (error Conflict "session revision does not match")
  else if Option.is_some state.active_operation
  then Error (error Conflict "session has an active foreground operation")
  else (
    match state.lifecycle.observed with
    | Agent_protocol.Session.Stopped -> Ok ()
    | Queued_for_slot
    | Starting
    | Recovering
    | Idle
    | Running_turn _
    | Compacting _
    | Waiting_for_permission _
    | Stopping
    | Failed _ ->
      Error (error Invalid_state "administrative mutation requires a stopped session"))
;;

let reset_cache t entry =
  match entry.Session_registry.store_handle with
  | None -> Error (error Invalid_state "session has no durable cache directory")
  | Some handle ->
    let path = Agent_store.Session_store.Handle.cache_directory handle in
    (try
       let cache = Eio.Path.(Eio.Stdenv.fs t.env / path) in
       Eio.Path.rmtree ~missing_ok:true cache;
       Eio.Path.mkdir ~perm:0o700 cache;
       Ok ()
     with
     | exn ->
       Agent_store.Store_error.of_exn ~operation:"reset session cache" ~path exn
       |> persistence_error
       |> Result.fail)
;;

let replacement_workspace t entry state keep_workspace =
  if keep_workspace
  then Ok None
  else (
    match state.Agent_session.Session_state.spec.workspace_instance.source_kind with
    | Temporary _ -> Result.map (replacement_temporary t entry state) ~f:Option.some
    | Physical | Current -> Ok None)
;;

let handle_session_reset t context command_audit request =
  with_writer
    t
    context
    ~session_id:request.Agent_protocol.Session.Reset_request.session_id
    ~attachment_id:request.attachment_id
    (fun entry ->
       let open Result.Let_syntax in
       let%bind state = Agent_session.Session_actor.state entry.actor in
       let%bind () = validate_stopped_revision state request.expected_revision in
       let%bind () = Runtime_owner.unload entry.runtime in
       let%bind workspace_instance =
         replacement_workspace t entry state request.keep_workspace
       in
       let%bind () = if request.keep_cache then Ok () else reset_cache t entry in
       let options =
         Agent_session.Session_actor.
           { keep_history = request.keep_history
           ; keep_tasks = request.keep_tasks
           ; keep_grants = request.keep_grants
           ; keep_labels = request.keep_labels
           ; workspace_instance
           }
       in
       let%bind _ =
         actor_command
           command_audit
           ~plain:(fun () ->
             Agent_session.Session_actor.reset
               entry.actor
               ~attachment_id:request.attachment_id
               ~expected_revision:request.expected_revision
               options)
           ~audited:(fun command_audit ->
             Agent_session.Session_actor.reset_with_command_audit
               entry.actor
               ~command_audit
               ~attachment_id:request.attachment_id
               ~expected_revision:request.expected_revision
               options)
       in
       let%bind () =
         Option.value_map workspace_instance ~default:(Ok ()) ~f:(fun instance ->
           update_workspace_capacity entry instance)
       in
       let%map state = Agent_session.Session_actor.state entry.actor in
       Agent_protocol.Method_result.Session_reset
         (session_mutation (Agent_session.Session_state.summary state)))
;;

let current_prompt_revision t state =
  let open Result.Let_syntax in
  let%bind definition_id =
    state.Agent_session.Session_state.spec.prompt_definition_id
    |> Result.of_option
         ~error:(error Prompt_unavailable "session has no prompt definition")
  in
  let%bind entry =
    Agent_session.Prompt_catalog.find t.prompts definition_id
    |> Result.of_option ~error:(error Prompt_not_found "prompt is not in the catalog")
  in
  match entry.availability with
  | Agent_session.Prompt_catalog.Ready revision -> Ok revision
  | Disabled | Unavailable _ -> Error (error Prompt_unavailable "prompt is unavailable")
;;

let restore_target_revision t state target_revision =
  let open Result.Let_syntax in
  let%bind definition_id =
    state.Agent_session.Session_state.spec.prompt_definition_id
    |> Result.of_option
         ~error:(error Prompt_unavailable "session has no prompt definition")
  in
  Agent_session.Prompt_catalog.restore_revision
    t.prompts
    ~definition_id
    ~revision_id:target_revision
  |> Result.map_error ~f:(fun diagnostics ->
    error
      Prompt_unavailable
      (List.map diagnostics ~f:(fun diagnostic -> diagnostic.message)
       |> String.concat ~sep:"\n"))
;;

let commit_prepared
      t
      entry
      command_audit
      ~attachment_id
      ~expected_revision
      ~kind
      ~fresh_history
      candidate
  =
  Runtime_owner.with_administration entry.Session_registry.runtime (fun () ->
    let open Result.Let_syntax in
    let%bind candidate = t.prepare_administration entry candidate ~fresh_history in
    let%map session =
      Agent_session.Session_actor.commit_administration
        entry.actor
        ~command_audit
        ~attachment_id
        ~expected_revision
        ~kind
        candidate
    in
    Agent_session.History_id_source.discard_reserved entry.history_ids;
    session)
;;

let handle_session_rebuild t context command_audit request =
  with_writer
    t
    context
    ~session_id:request.Agent_protocol.Session.Rebuild_request.session_id
    ~attachment_id:request.attachment_id
    (fun entry ->
       let open Result.Let_syntax in
       let%bind state = Agent_session.Session_actor.state entry.actor in
       let%bind () = validate_stopped_revision state request.expected_revision in
       let%bind target =
         match request.prompt_choice with
         | Agent_protocol.Session.Rebuild_request.Pinned ->
           Ok state.spec.prompt_revision_id
         | Current_catalog ->
           Result.map
             (current_prompt_revision t state)
             ~f:Agent_session.Prompt_revision.id
       in
       let%bind candidate = Agent_session.Administration.rebuild state target in
       let%map session =
         commit_prepared
           t
           entry
           command_audit
           ~attachment_id:request.attachment_id
           ~expected_revision:request.expected_revision
           ~kind:Rebuild
           ~fresh_history:true
           candidate
       in
       Agent_protocol.Method_result.Session_rebuild (session_mutation session))
;;

let handle_session_upgrade_prompt t context command_audit request =
  with_writer
    t
    context
    ~session_id:request.Agent_protocol.Session.Upgrade_prompt_request.session_id
    ~attachment_id:request.attachment_id
    (fun entry ->
       let open Result.Let_syntax in
       let%bind state = Agent_session.Session_actor.state entry.actor in
       let%bind () = validate_stopped_revision state request.expected_revision in
       let%bind (_ : Agent_session.Prompt_revision.t) =
         restore_target_revision t state request.target_revision
       in
       let%bind () =
         if Option.is_some state.moderator && not request.allow_migration
         then
           Error (error Conflict "prompt upgrade requires moderator migration approval")
         else Ok ()
       in
       let candidate =
         Agent_session.Administration.upgrade state request.target_revision
       in
       let%map session =
         commit_prepared
           t
           entry
           command_audit
           ~attachment_id:request.attachment_id
           ~expected_revision:request.expected_revision
           ~kind:Upgrade
           ~fresh_history:false
           candidate
       in
       Agent_protocol.Method_result.Session_upgrade_prompt (session_mutation session))
;;

let handle_permission_list t context request =
  let open Result.Let_syntax in
  let%bind entry, _ =
    find_visible_entry t context request.Agent_protocol.Permission.List_request.session_id
  in
  let%map state = Agent_session.Session_actor.state entry.actor in
  let permissions =
    List.filter state.permissions ~f:(fun permission ->
      Option.value_map request.state ~default:true ~f:(fun requested ->
        Agent_protocol.Permission.equal_state requested permission.state))
  in
  Agent_protocol.Method_result.Permission_list (page request.page.limit permissions)
;;

let handle_permission_respond t context command_audit request =
  let open Result.Let_syntax in
  with_writer
    t
    context
    ~session_id:request.Agent_protocol.Permission.Respond_request.session_id
    ~attachment_id:request.attachment_id
    (fun entry ->
       let%bind permission =
         actor_command
           command_audit
           ~plain:(fun () ->
             Agent_session.Session_actor.respond_permission
               entry.actor
               ~attachment_id:request.attachment_id
               ~principal_id:(Some (Connection_context.principal context).id)
               ~permission_id:request.permission_id
               ~permission_generation:request.permission_generation
               ~choice:request.choice
               ~reason:request.reason)
           ~audited:(fun command_audit ->
             Agent_session.Session_actor.respond_permission_with_command_audit
               entry.actor
               ~command_audit
               ~attachment_id:request.attachment_id
               ~principal_id:(Some (Connection_context.principal context).id)
               ~permission_id:request.permission_id
               ~permission_generation:request.permission_generation
               ~choice:request.choice
               ~reason:request.reason)
       in
       let%map state = Agent_session.Session_actor.state entry.actor in
       let session = Agent_session.Session_state.summary state in
       Agent_protocol.Method_result.Permission_respond
         { permission; mutation = mutation session })
;;

let visible_loaded_states t context =
  let principal = Connection_context.principal context in
  Session_registry.load_all t.registry
  |> Result.bind ~f:(fun entries ->
    List.map entries ~f:(fun entry -> Agent_session.Session_actor.state entry.actor)
    |> Result.all)
  |> Result.map ~f:(List.filter ~f:(state_visible_to principal))
;;

let handle_grant_list t context request =
  let open Result.Let_syntax in
  let%map grants =
    let%map states = visible_loaded_states t context in
    states
    |> List.concat_map ~f:(fun state ->
      Agent_session.Security_grant.list
        ~now:(now t)
        ~session_id:state.identity.session_id
        ~creating_principal:state.identity.creating_principal
        ~generic:state.grants
        ~shell:state.shell)
    |> List.filter ~f:(fun grant ->
      Option.value_map
        request.Agent_protocol.Grant.List_request.session_id
        ~default:true
        ~f:(fun id -> Agent_protocol.Id.Session.compare id grant.session_id = 0)
      && Option.value_map request.principal_id ~default:true ~f:(fun id ->
        Agent_protocol.Id.Principal.compare id grant.principal_id = 0)
      && Option.value_map request.state ~default:true ~f:(fun state ->
        Agent_protocol.Grant.equal_state state grant.state))
  in
  Agent_protocol.Method_result.Grant_list (page request.page.limit grants)
;;

let handle_grant_revoke t context command_audit request =
  let open Result.Let_syntax in
  with_writer
    t
    context
    ~session_id:request.Agent_protocol.Grant.Revoke_request.session_id
    ~attachment_id:request.attachment_id
    (fun entry ->
       let%bind grant, session =
         actor_command
           command_audit
           ~plain:(fun () ->
             Agent_session.Session_actor.revoke_grant
               entry.actor
               ~attachment_id:request.attachment_id
               ~grant_id:request.grant_id
               ~reason:request.reason)
           ~audited:(fun command_audit ->
             Agent_session.Session_actor.revoke_grant_with_command_audit
               entry.actor
               ~command_audit
               ~attachment_id:request.attachment_id
               ~grant_id:request.grant_id
               ~reason:request.reason)
       in
       Ok
         (Agent_protocol.Method_result.Grant_revoke { grant; mutation = mutation session }))
;;

let handle_audit_read t request =
  Agent_store.Audit_store.read t.audit_store request
  |> Result.map_error ~f:audit_read_error
  |> Result.map ~f:(fun page -> Agent_protocol.Method_result.Audit_read page)
;;

let job_status_name = function
  | Agent_protocol.Job.Queued -> "queued"
  | Running -> "running"
  | Waiting_permission _ -> "waiting_permission"
  | Waiting_completion _ -> "waiting_completion"
  | Succeeded -> "succeeded"
  | Failed _ -> "failed"
  | Cancelled -> "cancelled"
  | Interrupted _ -> "interrupted"
;;

let find_job state job_id =
  List.find state.Agent_session.Session_state.jobs ~f:(fun job ->
    Agent_protocol.Id.Job.compare job.id job_id = 0)
  |> Result.of_option ~error:(error Invalid_request "job does not exist")
;;

let handle_job_list t context request =
  let open Result.Let_syntax in
  let%bind entry, _ =
    find_visible_entry t context request.Agent_protocol.Job.List_request.session_id
  in
  let%map state = Agent_session.Session_actor.state entry.actor in
  let jobs =
    List.filter state.jobs ~f:(fun job ->
      Option.value_map request.status ~default:true ~f:(fun status ->
        String.equal status (job_status_name job.status))
      && Option.value_map request.kind ~default:true ~f:(fun kind ->
        Agent_protocol.Job.equal_kind kind job.kind))
  in
  Agent_protocol.Method_result.Job_list (page request.page.limit jobs)
;;

let handle_job_get t context request =
  let open Result.Let_syntax in
  let%bind entry, _ =
    find_visible_entry t context request.Agent_protocol.Job.Get_request.session_id
  in
  let%map job = Agent_session.Session_actor.read_job entry.actor ~job_id:request.job_id in
  Agent_protocol.Method_result.Job_get job
;;

let handle_job_cancel t context command_audit request =
  let open Result.Let_syntax in
  with_writer
    t
    context
    ~session_id:request.Agent_protocol.Job.Cancel_request.session_id
    ~attachment_id:request.attachment_id
    (fun entry ->
       let%bind job =
         actor_command
           command_audit
           ~plain:(fun () ->
             Agent_session.Session_actor.cancel_job
               entry.actor
               ~attachment_id:request.attachment_id
               ~job_id:request.job_id
               ())
           ~audited:(fun command_audit ->
             Agent_session.Session_actor.cancel_job
               entry.actor
               ~command_audit
               ~attachment_id:request.attachment_id
               ~job_id:request.job_id
               ())
       in
       t.cancel_job job.id;
       let%map state = Agent_session.Session_actor.state entry.actor in
       let session = Agent_session.Session_state.summary state in
       Agent_protocol.Method_result.Job_cancel { job; mutation = mutation session })
;;

let schedule_status_name = function
  | Agent_protocol.Schedule.Scheduled -> "scheduled"
  | Delivering -> "delivering"
  | Delivered -> "delivered"
  | Cancelled -> "cancelled"
  | Failed _ -> "failed"
;;

let find_schedule state schedule_id =
  List.find state.Agent_session.Session_state.schedules ~f:(fun schedule ->
    Agent_protocol.Id.Schedule.compare schedule.id schedule_id = 0)
  |> Result.of_option ~error:(error Invalid_request "schedule does not exist")
;;

let handle_schedule_list t context request =
  let open Result.Let_syntax in
  let%bind entry, _ =
    find_visible_entry t context request.Agent_protocol.Schedule.List_request.session_id
  in
  let%map state = Agent_session.Session_actor.state entry.actor in
  let schedules =
    List.filter state.schedules ~f:(fun schedule ->
      Option.value_map request.status ~default:true ~f:(fun status ->
        String.equal status (schedule_status_name schedule.status)))
  in
  Agent_protocol.Method_result.Schedule_list (page request.page.limit schedules)
;;

let handle_schedule_get t context request =
  let open Result.Let_syntax in
  let%bind entry, _ =
    find_visible_entry t context request.Agent_protocol.Schedule.Get_request.session_id
  in
  let%bind state = Agent_session.Session_actor.state entry.actor in
  let%map schedule = find_schedule state request.schedule_id in
  Agent_protocol.Method_result.Schedule_get schedule
;;

let schedule_due now = function
  | Agent_protocol.Schedule.At timestamp -> timestamp
  | After_ms delay ->
    Agent_protocol.Timestamp.to_time_ns now
    |> Fn.flip Time_ns.add (Time_ns.Span.of_ms (Float.of_int delay))
    |> Agent_protocol.Timestamp.of_time_ns
;;

let handle_schedule_create t context command_audit request =
  let open Result.Let_syntax in
  with_writer
    t
    context
    ~session_id:request.Agent_protocol.Schedule.Create_request.session_id
    ~attachment_id:request.attachment_id
    (fun entry ->
       let%bind state = Agent_session.Session_actor.state entry.actor in
       let now = now t in
       let schedule =
         Agent_protocol.Schedule.
           { id = Agent_protocol.Id.Schedule.create ()
           ; session_id = request.session_id
           ; generation = state.identity.generation
           ; payload = request.payload
           ; created_at = now
           ; next_due_at = schedule_due now request.due
           ; misfire = request.misfire
           ; status = Scheduled
           ; delivery_count = 0
           ; last_delivery_at = None
           ; delivery_cancellation = None
           ; ownership = None
           }
       in
       let%map session =
         actor_command
           command_audit
           ~plain:(fun () ->
             Agent_session.Session_actor.change_schedule
               entry.actor
               ~attachment_id:request.attachment_id
               ~event:`Created
               schedule)
           ~audited:(fun command_audit ->
             Agent_session.Session_actor.change_schedule_with_command_audit
               entry.actor
               ~command_audit
               ~attachment_id:request.attachment_id
               ~event:`Created
               schedule)
       in
       Agent_protocol.Method_result.Schedule_create
         { schedule; mutation = mutation session })
;;

let handle_schedule_cancel t context command_audit request =
  let open Result.Let_syntax in
  with_writer
    t
    context
    ~session_id:request.Agent_protocol.Schedule.Cancel_request.session_id
    ~attachment_id:request.attachment_id
    (fun entry ->
       let%bind state = Agent_session.Session_actor.state entry.actor in
       let%bind schedule = find_schedule state request.schedule_id in
       match schedule.status with
       | Cancelled | Delivered | Delivering | Failed _ ->
         Error (error Already_resolved "schedule is already terminal")
       | Scheduled ->
         let schedule = { schedule with status = Cancelled } in
         let%map session =
           actor_command
             command_audit
             ~plain:(fun () ->
               Agent_session.Session_actor.change_schedule
                 entry.actor
                 ~attachment_id:request.attachment_id
                 ~event:`Cancelled
                 schedule)
             ~audited:(fun command_audit ->
               Agent_session.Session_actor.change_schedule_with_command_audit
                 entry.actor
                 ~command_audit
                 ~attachment_id:request.attachment_id
                 ~event:`Cancelled
                 schedule)
         in
         Agent_protocol.Method_result.Schedule_cancel
           { schedule; mutation = mutation session })
;;

let mutation_attachment = function
  | Agent_protocol.Command.Session_start r -> Some (r.session_id, r.attachment_id)
  | Session_stop r -> Some (r.session_id, r.attachment_id)
  | Session_cancel_operation r -> Some (r.session_id, r.attachment_id)
  | Session_send_message r -> Some (r.session_id, r.attachment_id)
  | Session_compact r -> Some (r.session_id, r.attachment_id)
  | Session_delete_history r -> Some (r.session_id, r.attachment_id)
  | Session_reset r -> Some (r.session_id, r.attachment_id)
  | Session_rebuild r -> Some (r.session_id, r.attachment_id)
  | Session_upgrade_prompt r -> Some (r.session_id, r.attachment_id)
  | Session_delete r -> Some (r.session_id, r.attachment_id)
  | Permission_respond r -> Some (r.session_id, r.attachment_id)
  | Grant_revoke r -> Some (r.session_id, r.attachment_id)
  | Job_cancel r -> Some (r.session_id, r.attachment_id)
  | Schedule_create r -> Some (r.session_id, r.attachment_id)
  | Schedule_cancel r -> Some (r.session_id, r.attachment_id)
  | _ -> None
;;

let authorize_mutation t context command =
  match mutation_attachment command with
  | None -> Ok ()
  | Some (session_id, attachment_id) ->
    with_writer t context ~session_id ~attachment_id (fun _ -> Ok ())
;;

let dispatch_authorized t ~context ~command_audit = function
  | Agent_protocol.Command.Protocol_initialize request ->
    Result.map
      (t.initialize ~principal:(Connection_context.principal context) request)
      ~f:(fun response ->
        Connection_context.mark_initialized context;
        Agent_protocol.Method_result.Protocol_initialize response)
  | Protocol_ping request ->
    Ok (Agent_protocol.Method_result.Protocol_ping (t.ping request))
  | Server_info -> Ok (Agent_protocol.Method_result.Server_info (t.server_info ()))
  | Server_health request ->
    let include_details =
      request.Agent_protocol.Health.Request.include_details
      && Agent_protocol.Principal.has_scope
           (Connection_context.principal context)
           Diagnostics
    in
    let request = Agent_protocol.Health.Request.{ include_details } in
    Ok (Agent_protocol.Method_result.Server_health (t.server_health request))
  | Prompt_list request -> handle_prompt_list t request
  | Prompt_get request -> handle_prompt_get t request
  | Workspace_list request -> handle_workspace_list t request
  | Workspace_get request -> handle_workspace_get t request
  | Blob_read request -> handle_blob_read t context request
  | Session_create request -> handle_session_create t context command_audit request
  | Session_list request -> handle_session_list t context request
  | Session_get request -> handle_session_get t context request
  | Session_attach request -> handle_session_attach t context command_audit request
  | Session_detach request -> handle_session_detach t context command_audit request
  | Session_renew_owner request ->
    handle_session_renew_owner t context command_audit request
  | Session_start request -> handle_session_start t context command_audit request
  | Session_stop request -> handle_session_stop t context command_audit request
  | Session_cancel_operation request ->
    handle_session_cancel_operation t context command_audit request
  | Session_send_message request -> handle_send_message t context command_audit request
  | Session_compact request -> handle_session_compact t context command_audit request
  | Session_delete_history request ->
    handle_delete_history t context command_audit request
  | Session_export request -> handle_session_export t context request
  | Session_reset request -> handle_session_reset t context command_audit request
  | Session_rebuild request -> handle_session_rebuild t context command_audit request
  | Session_upgrade_prompt request ->
    handle_session_upgrade_prompt t context command_audit request
  | Session_delete request -> handle_session_delete t context request
  | Permission_list request -> handle_permission_list t context request
  | Permission_respond request ->
    handle_permission_respond t context command_audit request
  | Grant_list request -> handle_grant_list t context request
  | Grant_revoke request -> handle_grant_revoke t context command_audit request
  | Audit_read request -> handle_audit_read t request
  | Job_list request -> handle_job_list t context request
  | Job_get request -> handle_job_get t context request
  | Job_cancel request -> handle_job_cancel t context command_audit request
  | Schedule_list request -> handle_schedule_list t context request
  | Schedule_get request -> handle_schedule_get t context request
  | Schedule_create request -> handle_schedule_create t context command_audit request
  | Schedule_cancel request -> handle_schedule_cancel t context command_audit request
;;

let handle_authorized t ~context ~command_audit command =
  let open Result.Let_syntax in
  let%bind () = authorize_mutation t context command in
  let%bind result = dispatch_authorized t ~context ~command_audit command in
  Pagination.lists t.pagination (Connection_context.principal context) command result
;;

let command_session_id = function
  | Agent_protocol.Command.Session_create _
  | Protocol_initialize _
  | Protocol_ping _
  | Server_info
  | Server_health _
  | Prompt_list _
  | Prompt_get _
  | Workspace_list _
  | Workspace_get _
  | Session_list _ -> None
  | Blob_read request -> Some request.session_id
  | Audit_read request -> request.session_id
  | Session_get request -> Some request.Agent_protocol.Session.Get_request.session_id
  | Session_attach request -> Some request.session_id
  | Session_detach request -> Some request.session_id
  | Session_renew_owner request -> Some request.session_id
  | Session_start request -> Some request.session_id
  | Session_stop request -> Some request.session_id
  | Session_cancel_operation request -> Some request.session_id
  | Session_send_message request -> Some request.session_id
  | Session_compact request -> Some request.session_id
  | Session_delete_history request -> Some request.session_id
  | Session_export request -> Some request.session_id
  | Session_reset request -> Some request.session_id
  | Session_rebuild request -> Some request.session_id
  | Session_upgrade_prompt request -> Some request.session_id
  | Session_delete request -> Some request.session_id
  | Permission_list request -> Some request.session_id
  | Permission_respond request -> Some request.session_id
  | Grant_list request -> request.session_id
  | Grant_revoke request -> Some request.session_id
  | Job_list request -> Some request.session_id
  | Job_get request -> Some request.session_id
  | Job_cancel request -> Some request.session_id
  | Schedule_list request -> Some request.session_id
  | Schedule_get request -> Some request.session_id
  | Schedule_create request -> Some request.session_id
  | Schedule_cancel request -> Some request.session_id
;;

let audit_outcome
      t
      context
      command
      (outcome : (Agent_protocol.Method_result.t, Agent_protocol.Error.t) result)
  =
  let method_name = Agent_protocol.Command.method_name command in
  let level, name, outcome_payload =
    match outcome with
    | Ok _ -> Agent_protocol.Audit.Info, "protocol.command.succeeded", `String "success"
    | Error failure ->
      ( Agent_protocol.Audit.Warning
      , "protocol.command.failed"
      , `String (Agent_protocol.Error.code_to_string failure.code) )
  in
  Agent_store.Audit_store.append
    t.audit_store
    ~timestamp:(now t)
    ~level
    ~name
    ~session_id:(command_session_id command)
    ~principal_id:(Some (Connection_context.principal context).id)
    ~payload:(`Object [ "method", `String method_name; "outcome", outcome_payload ])
    ~redacted:true
  |> Result.map_error ~f:persistence_error
;;

let execute t context command =
  match idempotency command with
  | None -> handle_authorized t ~context ~command_audit:None command
  | Some identity ->
    handle_idempotent t context command identity (fun command_audit ->
      handle_authorized t ~context ~command_audit command)
;;

let handle t ~context command =
  let open Result.Let_syntax in
  let%bind () = Authorization.authorize (Connection_context.principal context) command in
  if
    (not (Connection_context.initialized context))
    && not
         (String.equal (Agent_protocol.Command.method_name command) "protocol.initialize")
  then Error (error Incompatible_protocol "connection must initialize first")
  else (
    let outcome =
      Result.map
        (execute t context command)
        ~f:(Principal_projection.result (Connection_context.principal context))
    in
    match audit_outcome t context command outcome with
    | Ok _ -> outcome
    | Error audit_failure ->
      (match outcome with
       | Error _ -> outcome
       | Ok _ -> Error audit_failure))
;;

let close_connection t context =
  Connection_context.attachments context
  |> List.iter ~f:(fun attachment ->
    Option.iter (Session_registry.find t.registry attachment.session_id) ~f:(fun entry ->
      ignore
        (Agent_session.Session_actor.detach entry.actor attachment.id
         : (unit, Agent_protocol.Error.t) result));
    Connection_context.remove_attachment context attachment.id)
;;
