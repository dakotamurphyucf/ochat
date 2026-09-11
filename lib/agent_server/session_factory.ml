open! Core

type limits =
  { max_journal_payload : int
  ; max_segment_bytes : int64
  ; max_segment_frames : int
  ; commit_queue_capacity : int
  ; mailbox_capacity : int
  ; snapshot_payload_limit : int
  ; snapshot_every_events : int
  ; snapshot_every_ms : int
  ; moderator_reservation_size : int
  ; history_block_size : int
  ; owner_lease_duration_ms : int
  ; event_replay_capacity : int
  ; max_attachments_per_session : int
  ; subscriber_queue_capacity : int
  ; job_result_inline_bytes : int
  ; job_result_max_bytes : int
  ; job_result_recovery_max_count : int
  ; job_result_recovery_max_bytes : int
  ; delegation_recovery_max_count : int
  ; delegation_recovery_max_bytes : int
  ; delegation_max_depth : int
  ; job_result_collection : Agent_store.Job_result_store.Publisher.collection_limits
  ; subscriptions : Agent_session.Staged_subscriptions.limits
  ; schedules : Agent_session.Staged_schedules.limits
  ; notifications : Agent_session.Staged_notifications.limits
  ; ingress : Agent_session.Staged_ingress.limits
  }

type t =
  { sw : Eio.Switch.t
  ; env : Eio_unix.Stdenv.base
  ; store : Agent_store.Session_store.t
  ; registry : Session_registry.t
  ; generated_creation_mutex : Eio.Mutex.t
  ; idempotency_store : Agent_store.Idempotency_store.t
  ; blob_store : Agent_store.Blob_store.t
  ; prompts : Agent_session.Prompt_catalog.t
  ; workspaces : Agent_session.Workspace_catalog.t
  ; catalog_mutex : Eio.Mutex.t
  ; mutable permission_profiles : (string, Agent_session.Permission_policy.t) Map.Poly.t
  ; mutable permission_profile_revisions :
      (string, Agent_session.Permission_policy.t) Map.Poly.t
  ; mutable manifest_grants : Operator_manifest_grant.t list
  ; quota_manager : Agent_session.Quota_manager.t
  ; job_capacity : Job_capacity.t
  ; tool_dir : string
  ; home : string
  ; model_post_stream : Agent_session.Runtime_builder.model_post_stream option
  ; qualify_chatml_extensions : bool
  ; chatml_runtime_policy : Chat_response.Runtime_semantics.policy
  ; authoring_validation_host : Chat_response.Authoring_validation.host option
  ; durability : Agent_store.Journal_segment.durability
  ; limits : limits
  }

module Legacy_provenance = struct
  type t =
    { version : int
    ; source_id : string
    ; source_path : string
    ; prompt_file : string
    ; local_prompt_copy : string option
    ; vfs_root : string
    ; imported_at : Agent_protocol.Timestamp.t
    ; diagnostics : string list
    }
  [@@deriving sexp]
end

type runtime_source =
  | Authored of Agent_session.Prompt_revision.t
  | Generated_artifact of
      { artifact : Agent_store.Prompt_artifact_store.Artifact.t
      ; materialized_tree : Eio.Fs.dir_ty Eio.Path.t
      }

let create
      ~sw
      ~env
      ~store
      ~registry
      ~idempotency_store
      ~blob_store
      ~prompts
      ~workspaces
      ~permission_profiles
      ~manifest_grants
      ~quota_manager
      ~job_capacity
      ~tool_dir
      ~home
      ~model_post_stream
      ~qualify_chatml_extensions
      ~chatml_runtime_policy
      ~authoring_validation_host
      ~durability
      ~limits
  =
  let permission_profiles_by_id =
    List.fold permission_profiles ~init:Map.Poly.empty ~f:(fun profiles profile ->
      Map.set profiles ~key:profile.Agent_session.Permission_policy.id ~data:profile)
  in
  let permission_profile_revisions =
    List.fold permission_profiles ~init:Map.Poly.empty ~f:(fun profiles profile ->
      Map.set
        profiles
        ~key:profile.Agent_session.Permission_policy.revision_digest
        ~data:profile)
  in
  { sw
  ; env
  ; store
  ; registry
  ; generated_creation_mutex = Eio.Mutex.create ()
  ; idempotency_store
  ; blob_store
  ; prompts
  ; workspaces
  ; catalog_mutex = Eio.Mutex.create ()
  ; permission_profiles = permission_profiles_by_id
  ; permission_profile_revisions
  ; manifest_grants
  ; quota_manager
  ; job_capacity
  ; tool_dir
  ; home
  ; model_post_stream
  ; qualify_chatml_extensions
  ; chatml_runtime_policy
  ; authoring_validation_host
  ; durability
  ; limits
  }
;;

let install_catalogs t ~workspaces ~permission_profiles ~manifest_grants =
  Agent_session.Workspace_catalog.install t.workspaces ~replacement:workspaces;
  Eio.Mutex.use_rw ~protect:true t.catalog_mutex (fun () ->
    t.permission_profiles
    <- List.fold permission_profiles ~init:Map.Poly.empty ~f:(fun profiles profile ->
         Map.set profiles ~key:profile.Agent_session.Permission_policy.id ~data:profile);
    t.permission_profile_revisions
    <- List.fold
         permission_profiles
         ~init:t.permission_profile_revisions
         ~f:(fun profiles profile ->
           Map.set
             profiles
             ~key:profile.Agent_session.Permission_policy.revision_digest
             ~data:profile);
    t.manifest_grants <- manifest_grants)
;;

let now t =
  Eio.Time.now (Eio.Stdenv.clock t.env)
  |> Time_ns.Span.of_sec
  |> Time_ns.of_span_since_epoch
  |> Agent_protocol.Timestamp.of_time_ns
;;

let protocol_of_store error =
  Agent_protocol.Error.create
    Persistence_error
    ~message:(Sexp.to_string_hum ([%sexp_of: Agent_store.Store_error.t] error))
    ~retryable:false
    ()
;;

let unavailable code message =
  Agent_protocol.Error.create code ~message ~retryable:false ()
;;

let resolve_prompt t = function
  | Agent_protocol.Session.Prompt_ref.Generated _ ->
    Error
      (unavailable
         Permission_denied
         "generated sessions require scoped delegation admission")
  | Agent_protocol.Session.Prompt_ref.Local_path _ ->
    Error (unavailable Prompt_unavailable "daemon sessions require a catalog prompt")
  | Catalog prompt_id ->
    (match Agent_session.Prompt_catalog.find t.prompts prompt_id with
     | None -> Error (unavailable Prompt_not_found "prompt is not in the catalog")
     | Some { availability = Ready revision; definition } -> Ok (definition, revision)
     | Some { availability = Disabled; _ } ->
       Error (unavailable Prompt_unavailable "prompt is disabled")
     | Some { availability = Unavailable _; _ } ->
       Error (unavailable Prompt_unavailable "prompt revision is unavailable"))
;;

let resolve_workspace_definition t definition = function
  | Agent_protocol.Session.Workspace_request.Configured workspace_id ->
    let open Result.Let_syntax in
    let%bind workspace =
      Agent_session.Workspace_catalog.find t.workspaces workspace_id
      |> Result.of_option
           ~error:(unavailable Workspace_not_found "workspace is not in the catalog")
    in
    if Agent_session.Prompt_definition.allows_workspace definition workspace_id
    then Ok workspace
    else Error (unavailable Permission_denied "prompt is not allowed in this workspace")
  | Current | Local_path _ ->
    Error
      (unavailable Workspace_unavailable "daemon sessions require a configured workspace")
;;

let permission_profile t id =
  Eio.Mutex.use_ro t.catalog_mutex (fun () -> Map.find t.permission_profiles id)
  |> Result.of_option
       ~error:(unavailable Configuration_invalid "permission profile is unavailable")
;;

let permission_profile_revision t digest =
  Eio.Mutex.use_ro t.catalog_mutex (fun () ->
    Map.find t.permission_profile_revisions digest)
  |> Result.of_option
       ~error:
         (unavailable
            Configuration_invalid
            "the session's pinned permission profile revision is unavailable")
;;

let rewrite_session_workspace_path ~final_directory instance =
  match instance.Agent_session.Workspace_instance.source_kind with
  | Temporary Session_dir ->
    let parent = Eio_posix.Low_level.realpath (Filename.dirname final_directory) in
    let final_directory = Filename.concat parent (Filename.basename final_directory) in
    let native_path = Filename.concat final_directory "workspace" in
    { instance with
      canonical_root = { instance.canonical_root with native_path }
    ; configured_root = Some native_path
    }
  | Physical | Current | Temporary System_tmp -> instance
;;

let session_identity principal request session_id now =
  Agent_session.Session_state.Identity.
    { session_id
    ; display_name = request.Agent_protocol.Session.Create_request.spec.display_name
    ; creating_principal = Some principal.Agent_protocol.Principal.id
    ; created_at = now
    ; updated_at = now
    ; labels = request.spec.labels
    ; generation = 0
    }
;;

let quota_key definition instance =
  Some
    Agent_session.Quota_key.
      { conflict_domain = instance.Agent_session.Workspace_instance.conflict_domain
      ; prompt_id = definition.Agent_session.Prompt_definition.id
      }
;;

let session_spec request definition revision instance profile =
  Agent_session.Session_state.Spec.
    { protocol = request.Agent_protocol.Session.Create_request.spec
    ; prompt_definition_id = Some definition.Agent_session.Prompt_definition.id
    ; delegation = None
    ; prompt_revision_id = Agent_session.Prompt_revision.id revision
    ; workspace_instance = instance
    ; permission_profile = profile.Agent_session.Permission_policy.id
    ; permission_profile_digest = profile.revision_digest
    ; runtime_policy = definition.runtime_policy
    ; quota_key = quota_key definition instance
    }
;;

let provisional_state
      principal
      request
      session_id
      now
      definition
      revision
      instance
      profile
  =
  Agent_session.Session_state.create
    ~identity:(session_identity principal request session_id now)
    ~spec:(session_spec request definition revision instance profile)
    ~initial_history:[]
;;

let task_state = function
  | Session.Task.Pending -> "pending"
  | In_progress -> "in_progress"
  | Done -> "done"
;;

let task_json (task : Session.Task.t) =
  `Object
    [ "id", `String task.id
    ; "title", `String task.title
    ; "state", `String (task_state task.state)
    ]
;;

let moderator_json legacy =
  if not (List.is_empty legacy.Session.moderator_state.extensions)
  then
    Error
      (unavailable
         Migration_required
         "legacy moderator extensions cannot be imported without a registered codec")
  else (
    match
      legacy.moderator_state.identity_snapshot, legacy.moderator_state.legacy_snapshot
    with
    | Some snapshot, _ ->
      Ok
        (Some
           (`Object
               [ ( "identity_snapshot_sexp"
                 , `String
                     (Sexp.to_string_mach
                        ([%sexp_of: Session.Moderator_state.Identity_snapshot.t] snapshot))
                 )
               ]))
    | None, None -> Ok None
    | None, Some _ ->
      Error
        (unavailable
           Migration_required
           "legacy moderator state lacks an identity-safe checkpoint"))
;;

let imported_state provisional legacy =
  let open Result.Let_syntax in
  let%bind moderator = moderator_json legacy in
  let conversation =
    { provisional.Agent_session.Session_state.conversation with
      canonical_history = Agent_session.History_codec.all_to_protocol legacy.history
    ; initial_prompt_entry_count = 0
    ; next_history_sequence = Int64.of_int legacy.next_history_sequence
    ; reserved_history_through = Int64.of_int legacy.next_history_sequence
    ; tasks = List.map legacy.tasks ~f:task_json
    ; kv_store = legacy.kv_store
    }
  in
  let state = { provisional with conversation; moderator; shell = legacy.shell_state } in
  let%map () = Agent_session.Session_state.validate state in
  state
;;

let metadata state =
  Agent_store.Session_store.Metadata.
    { schema_version = Agent_store.Session_store.current_schema_version
    ; session = Agent_session.Session_state.summary state
    ; prompt_artifact =
        Agent_protocol.Id.Prompt_revision.to_string state.spec.prompt_revision_id
    ; workspace_identity = state.spec.workspace_instance.conflict_domain
    ; data_schema_version = Agent_session.Session_state.current_schema_version
    }
;;

let initialize_layout
      t
      ~principal
      ~request
      ~session_id
      ~now
      ~definition
      ~revision
      ~workspace_definition
      ~profile
      ~final_directory
      ~on_state
      ~staging_directory
  =
  let open Result.Let_syntax in
  let instance_id = Agent_protocol.Id.Workspace_instance.create () in
  let%map instance =
    Agent_session.Workspace_resolver.resolve
      ~env:t.env
      ~instance_id
      ~session_directory:staging_directory
      workspace_definition
  in
  let instance = rewrite_session_workspace_path ~final_directory instance in
  let state =
    provisional_state
      principal
      request
      session_id
      now
      definition
      revision
      instance
      profile
  in
  on_state state;
  metadata state
;;

let prompt_directory revision =
  Filename.concat
    (Agent_session.Prompt_revision.materialized_tree revision |> Eio.Path.native_exn)
    (Agent_session.Prompt_revision.root_relative_path revision |> Filename.dirname)
;;

let source_prompt_directory = function
  | Authored revision -> prompt_directory revision
  | Generated_artifact { artifact; materialized_tree } ->
    Filename.concat
      (Eio.Path.native_exn materialized_tree)
      (Filename.dirname artifact.root_relative_path)
;;

let generated_parent ?(check_stop_epoch = true) t (state : Agent_session.Session_state.t) =
  let open Result.Let_syntax in
  let missing_host () =
    Error
      (unavailable
         Invalid_state
         "delegation.runtime_unavailable: a qualified live parent runtime host is \
          required")
  in
  match t.qualify_chatml_extensions, state.spec.delegation with
  | true, Some reference ->
    let%bind record =
      Agent_store.Delegation_store.resolve
        (Agent_store.Session_store.delegations t.store)
        reference
      |> Result.map_error ~f:protocol_of_store
    in
    let%bind () =
      match record.stage with
      | Linked -> Ok ()
      | Reserved | Artifact_installed | Child_installed ->
        Error
          (unavailable
             Permission_denied
             "delegation.not_linked: child management relationship is not committed")
    in
    (match Session_registry.find t.registry record.key.parent_session_id with
     | None -> missing_host ()
     | Some parent ->
       let%bind current = Agent_session.Session_actor.state parent.actor in
       let%bind fingerprint = Agent_session.Delegation_authority.fingerprint current in
       (match
          ( current.lifecycle.desired
          , current.lifecycle.observed
          , current.halted
          , current.failure
          , record.revocation
          , record.admission.lifetime )
        with
        | ( Running
          , (Idle | Running_turn _ | Waiting_for_permission _)
          , false
          , None
          , None
          , Owned )
          when String.equal fingerprint record.admission.authority_sha256
               && ((not check_stop_epoch)
                   || Int64.equal
                        current.stop_epoch
                        (Option.value
                           state.parent_stop_epoch
                           ~default:
                             (Option.value record.admission.parent_stop_epoch ~default:0L))
                  ) -> Ok (parent, record)
        | _ ->
          Error
            (unavailable
               Permission_denied
               "delegation.parent_authority: parent authority or lifetime does not \
                permit execution")))
  | _ -> missing_host ()
;;

let check_source_for_execution t state = function
  | Authored _ -> Ok ()
  | Generated_artifact _ -> Result.map (generated_parent t state) ~f:ignore
;;

type parent_stop_recovery =
  { reference : Agent_store.Delegation_store.Reference.t
  ; epoch : int64
  ; stop : bool
  }

let parent_stop_recovery t (state : Agent_session.Session_state.t) =
  let module D = Agent_store.Delegation_store in
  let open Result.Let_syntax in
  match state.spec.delegation with
  | None -> Ok None
  | Some reference ->
    let%bind record =
      D.resolve (Agent_store.Session_store.delegations t.store) reference
      |> Result.map_error ~f:protocol_of_store
    in
    (match record.admission.lifetime with
     | Independent _ -> Ok None
     | Owned ->
       let previous =
         Option.value
           state.parent_stop_epoch
           ~default:(Option.value record.admission.parent_stop_epoch ~default:0L)
       in
       let%bind parent =
         match Session_registry.find t.registry record.key.parent_session_id with
         | None -> Ok None
         | Some entry ->
           Agent_session.Session_actor.state entry.actor |> Result.map ~f:Option.some
       in
       let epoch =
         Option.value_map parent ~default:previous ~f:(fun parent -> parent.stop_epoch)
       in
       let%bind () =
         match Int64.(epoch < previous) with
         | false -> Ok ()
         | true ->
           Error
             (protocol_of_store
                (Agent_store.Store_error.Corrupt
                   "parent stop counter moved backwards during child recovery"))
       in
       let stop =
         Option.is_some record.revocation
         || Int64.(epoch > previous)
         || Option.value_map parent ~default:true ~f:(fun parent ->
           Agent_protocol.Session.equal_desired_state parent.lifecycle.desired Stopped
           || parent.halted
           || Option.is_some parent.failure)
       in
       Ok (Some { reference; epoch; stop }))
;;

let runtime_paths t handle source state =
  Agent_session.Runtime_paths.create
    ~env:t.env
    ~tool_dir:t.tool_dir
    ~workspace:
      state.Agent_session.Session_state.spec.workspace_instance.canonical_root.native_path
    ~prompt_dir:(source_prompt_directory source)
    ~session_dir:(Agent_store.Session_store.Handle.directory handle)
    ~cache_dir:(Agent_store.Session_store.Handle.cache_directory handle)
    ~home:t.home
  |> Result.map_error ~f:protocol_of_store
;;

let shell_permission_choices scopes =
  let choices =
    List.map scopes ~f:(function
      | Chatmd_shell_spec.Shell_spec.Once -> Agent_protocol.Permission.Approve_once
      | Exact_session -> Approve_session
      | Prefix_session -> Approve_prefix
      | Durable_exact -> Durable_exact)
  in
  List.dedup_and_sort
    (Agent_protocol.Permission.Deny :: choices)
    ~compare:Agent_protocol.Permission.compare_choice
;;

let permission_fallback profile choices =
  match profile.Agent_session.Permission_policy.fallback with
  | Fallback_allow
    when List.mem
           choices
           Agent_protocol.Permission.Approve_once
           ~equal:Agent_protocol.Permission.equal_choice ->
    Agent_protocol.Permission.Approve_once
  | Fallback_allow | Fallback_deny | Fallback_allow_if_policy | Fallback_reviewer _ ->
    Deny
;;

let permission_is_expired now (permission : Agent_protocol.Permission.t) =
  Agent_protocol.Permission.equal_state permission.state Pending
  && Option.value_map permission.expires_at ~default:false ~f:(fun expires_at ->
    Agent_protocol.Timestamp.compare expires_at now <= 0)
;;

let shell_permission_expiry t profile =
  Option.map profile.Agent_session.Permission_policy.approval_timeout_ms ~f:(fun value ->
    now t
    |> Agent_protocol.Timestamp.to_time_ns
    |> Fn.flip Time_ns.add (Time_ns.Span.of_ms (Float.of_int value))
    |> Agent_protocol.Timestamp.of_time_ns)
;;

let shell_permission_timeout profile =
  Option.map profile.Agent_session.Permission_policy.approval_timeout_ms ~f:(fun value ->
    Float.of_int value /. 1_000.)
;;

let shell_responder_available t state =
  List.exists state.Agent_session.Session_state.attachments ~f:(fun attachment ->
    match attachment.Agent_protocol.Session.Attachment.mode with
    | Read_write -> true
    | Owner_read_write ->
      Option.value_map attachment.owner_lease ~default:true ~f:(fun lease ->
        Option.is_none lease.disconnect_grace_until
        && Agent_protocol.Timestamp.compare lease.expires_at (now t) > 0)
    | Read_only -> false)
;;

let shell_broker_response (request : Shell_runtime.Approval_broker.ui_request) resolution =
  match resolution.Agent_protocol.Permission.choice with
  | Approve_once -> Shell_runtime.Approval_broker.Approve_once
  | Approve_session -> Approve_exact_session
  | Approve_prefix -> Approve_prefix_session request.request.identity.argv
  | Durable_exact -> Approve_durable_exact
  | Deny ->
    Deny (Option.value resolution.reason ~default:"shell command approval was denied")
;;

let shell_permission_owner (state : Agent_session.Session_state.t) =
  match Agent_session.Native_tool_invocation.current_scope () with
  | Active invocation
    when Agent_protocol.Id.Session.equal
           invocation.context.session_id
           state.identity.session_id
         && invocation.context.generation = state.identity.generation ->
    Ok (Agent_protocol.Permission.Invocation invocation.context.id)
  | Active _ | Expired ->
    Error (unavailable Invalid_state "shell invocation scope is stale or foreign")
  | Unbound ->
    state.active_operation
    |> Result.of_option
         ~error:(unavailable Invalid_state "shell permission has no executing owner")
    |> Result.map ~f:(fun operation -> Agent_protocol.Permission.Operation operation.id)
;;

let resolve_shell_permission t profile actor request ~review_on_timeout =
  let open Result.Let_syntax in
  let%bind state = Agent_session.Session_actor.state actor in
  let choices = shell_permission_choices request.Shell_runtime.Approval_broker.scopes in
  let fallback = permission_fallback profile choices in
  if not (shell_responder_available t state)
  then
    Ok
      (match fallback with
       | Approve_once -> Shell_runtime.Approval_broker.Approve_once
       | Deny -> Deny "no shell permission responder is available"
       | Approve_session | Approve_prefix | Durable_exact -> assert false)
  else (
    let%bind owner = shell_permission_owner state in
    let permission =
      Agent_protocol.Permission.
        { id = Agent_protocol.Id.Permission.create ()
        ; session_id = state.identity.session_id
        ; generation = state.identity.generation
        ; owner
        ; call_id = request.request.context.request_id
        ; tool_name = "shell:" ^ request.runtime_id
        ; runtime_identity = Some request.request.identity.command_hash
        ; invocation_display = request.request.display_command
        ; rationale = request.request.rationale
        ; effects = Shell_access.Effect.to_strings request.request.context.effects
        ; choices
        ; created_at = now t
        ; expires_at = shell_permission_expiry t profile
        ; state = Pending
        ; resolution = None
        }
    in
    (match profile.Agent_session.Permission_policy.fallback with
     | Fallback_reviewer _ ->
       Agent_session.Session_actor.request_permission_with_review_fallback
         actor
         ~permission
         ~timeout_seconds:(shell_permission_timeout profile)
         ~fallback
         ~review_on_timeout
     | Fallback_allow | Fallback_deny | Fallback_allow_if_policy ->
       Agent_session.Session_actor.request_permission
         actor
         ~permission
         ~timeout_seconds:(shell_permission_timeout profile)
         ~fallback)
    |> Result.map ~f:(shell_broker_response request))
;;

let interactive_shell_provider t profile actor_ref ~review_timeout =
  let broker_ref = ref None in
  let broker =
    Shell_runtime.Approval_broker.create
      ~on_pending:(fun request ->
        Eio.Fiber.fork ~sw:t.sw (fun () ->
          let response =
            match !actor_ref with
            | None -> Shell_runtime.Approval_broker.Deny "session actor is unavailable"
            | Some actor ->
              (match
                 resolve_shell_permission
                   t
                   profile
                   actor
                   request
                   ~review_on_timeout:(review_timeout request)
               with
               | Ok response -> response
               | Error error -> Deny error.message)
          in
          Option.iter !broker_ref ~f:(fun broker ->
            ignore
              (Shell_runtime.Approval_broker.respond broker ~id:request.id response
               : (unit, Shell_runtime.Approval_broker.error) result))))
      ()
  in
  broker_ref := Some broker;
  Shell_runtime.Approval_broker.Callback broker
;;

let shell_policy_invocation request =
  Agent_session.Permission_policy.
    { tool_name = "shell:" ^ request.Shell_runtime.Approval_broker.runtime_id
    ; identity_digest = request.request.identity.command_hash
    ; invocation_display = request.request.display_command
    ; effects = Shell_access.Effect.to_strings request.request.context.effects
    }
;;

let shell_user_id state =
  Option.map state.Agent_session.Session_state.identity.creating_principal ~f:(fun id ->
    Agent_protocol.Id.Principal.to_string id)
;;

let shell_store_error (error : Agent_protocol.Error.t) =
  Shell_runtime.Approval_store.
    { code = "shell.actor_store_failed"; message = error.message }
;;

let shell_approval_store state actor_ref shell_state =
  let load () =
    match !actor_ref with
    | None -> Ok !shell_state.Session.Shell_state.approval_grants
    | Some actor ->
      Agent_session.Session_actor.shell_approval_grants actor
      |> Result.map_error ~f:shell_store_error
  in
  let commit approval_grants =
    match !actor_ref with
    | None ->
      shell_state := { !shell_state with approval_grants };
      Ok ()
    | Some actor ->
      Agent_session.Session_actor.replace_shell_approval_grants actor approval_grants
      |> Result.map_error ~f:shell_store_error
  in
  Shell_runtime.Approval_store.create
    ~load
    ~commit
    ~bindings:
      Shell_runtime.Approval_store.{ user_id = shell_user_id state; host_id = None }
  |> Shell_runtime.Approval_store.executor_store
;;

let manifest_store_error (error : Agent_protocol.Error.t) =
  Shell_runtime.Manifest_grant_store.
    { code = "shell.manifest_actor_store_failed"; message = error.message }
;;

let manifest_source revision =
  let artifact = Agent_session.Prompt_revision.artifact revision in
  let canonical_source_root =
    Option.value_map
      artifact.canonical_source
      ~default:(prompt_directory revision)
      ~f:Filename.dirname
  in
  Shell_runtime.Manifest_grant_store.
    { canonical_source_root
    ; source_sha256 = artifact.root_sha256
    ; repository_identity = None
    }
;;

let manifest_now_ns t () =
  now t
  |> Agent_protocol.Timestamp.to_time_ns
  |> Time_ns.to_int63_ns_since_epoch
  |> Int63.to_int64
;;

let operator_manifest_authorizer t revision state =
  let artifact = Agent_session.Prompt_revision.artifact revision in
  fun request ->
    let open Shell_runtime.Manifest_authorizer in
    match
      ( state.Agent_session.Session_state.spec.prompt_definition_id
      , state.spec.workspace_instance.definition_id
      , state.identity.creating_principal
      , artifact.shell_manifest_sha256 )
    with
    | ( Some prompt_definition_id
      , Some workspace_definition_id
      , Some principal_id
      , Some hash )
      when String.equal hash request.manifest.sha256 ->
      let grants = Eio.Mutex.use_ro t.catalog_mutex (fun () -> t.manifest_grants) in
      if
        List.exists grants ~f:(fun grant ->
          Operator_manifest_grant.authorizes
            grant
            ~prompt_definition_id
            ~workspace_definition_id
            ~principal_id
            ~source_sha256:artifact.root_sha256
            ~manifest_sha256:request.manifest.sha256)
      then Authorize_once
      else Reject "no operator grant authorizes this exact shell manifest"
    | Some _, Some _, Some _, Some _ ->
      Reject "compiled shell manifest does not match the prompt artifact"
    | _ -> Reject "shell manifest cannot be bound to a complete session identity"
;;

let manifest_authorizer t profile revision state actor_ref shell_state =
  let load () =
    match !actor_ref with
    | None -> Ok !shell_state.Session.Shell_state.manifest_grants
    | Some actor ->
      Agent_session.Session_actor.shell_manifest_grants actor
      |> Result.map_error ~f:manifest_store_error
  in
  let remember grant =
    match !actor_ref with
    | None ->
      shell_state
      := { !shell_state with manifest_grants = grant :: !shell_state.manifest_grants };
      Ok ()
    | Some actor ->
      Agent_session.Session_actor.add_shell_manifest_grant actor grant
      |> Result.map_error ~f:manifest_store_error
  in
  let fallback =
    match profile.Agent_session.Permission_policy.manifest_authorization with
    | Require_grant -> operator_manifest_authorizer t revision state
    | Deny_manifest -> Shell_runtime.Manifest_authorizer.deny
    | Assume_authorized -> Shell_runtime.Manifest_authorizer.assume_authorized
  in
  Shell_runtime.Manifest_grant_store.authorizer
    ~load
    ~remember
    ~now_ns:(manifest_now_ns t)
    ~session_id:
      (Agent_protocol.Id.Session.to_string
         state.Agent_session.Session_state.identity.session_id)
    ~source:(manifest_source revision)
    ~bindings:
      Shell_runtime.Manifest_grant_store.{ user_id = shell_user_id state; host_id = None }
    ~fallback
;;

type pending_schedule_operation =
  | Add of Agent_protocol.Schedule.t
  | Cancel of Agent_protocol.Id.Schedule.t

let add_bound_schedule actor_ref pending schedule =
  match !actor_ref with
  | Some actor ->
    Agent_session.Session_actor.add_schedule actor schedule
    |> Result.map ~f:(fun schedule ->
      Agent_protocol.Id.Schedule.to_string schedule.Agent_protocol.Schedule.id)
    |> Result.map_error ~f:(fun error -> error.message)
  | None ->
    pending := !pending @ [ Add schedule ];
    Ok (Agent_protocol.Id.Schedule.to_string schedule.id)
;;

let schedule_services t state actor_ref pending =
  let session_id = state.Agent_session.Session_state.identity.session_id in
  let generation = state.identity.generation in
  let after_ms ~delay_ms ~payload =
    let open Result.Let_syntax in
    if delay_ms < 0
    then Error "schedule delay must be nonnegative"
    else (
      let created_at = now t in
      let%bind next_due_at =
        Agent_protocol.Timestamp.add_ms created_at delay_ms
        |> Result.map_error ~f:(fun error -> error.message)
      in
      let schedule =
        Agent_protocol.Schedule.
          { id = Agent_protocol.Id.Schedule.create ()
          ; session_id
          ; generation
          ; payload
          ; created_at
          ; next_due_at
          ; misfire = Deliver_once_immediately
          ; status = Scheduled
          ; delivery_count = 0
          ; last_delivery_at = None
          ; delivery_cancellation = None
          ; ownership = None
          }
      in
      add_bound_schedule actor_ref pending schedule)
  in
  let cancel ~id =
    let open Result.Let_syntax in
    let%bind schedule_id =
      Agent_protocol.Id.Schedule.of_string id
      |> Result.map_error ~f:(fun error -> error.message)
    in
    match !actor_ref with
    | Some actor ->
      Agent_session.Session_actor.cancel_schedule_internal actor ~schedule_id
      |> Result.map ~f:(fun _ -> ())
      |> Result.map_error ~f:(fun error -> error.message)
    | None ->
      pending := !pending @ [ Cancel schedule_id ];
      Ok ()
  in
  Agent_session.Runtime_builder.{ after_ms; cancel }
;;

let flush_pending_schedules actor operations =
  let apply = function
    | Add schedule ->
      Agent_session.Session_actor.add_schedule actor schedule
      |> Result.map ~f:(fun _ -> ())
    | Cancel schedule_id ->
      Agent_session.Session_actor.cancel_schedule_internal actor ~schedule_id
      |> Result.map ~f:(fun _ -> ())
  in
  List.fold_result operations ~init:() ~f:(fun () operation -> apply operation)
;;

let model_job_payload ~recipe ~payload =
  `Object [ "recipe", `String recipe; "payload", payload ]
;;

let model_job t state ~recipe ~payload ~delivery =
  Agent_protocol.Job.
    { id = Agent_protocol.Id.Job.create ()
    ; session_id = state.Agent_session.Session_state.identity.session_id
    ; generation = state.identity.generation
    ; kind = Model_call
    ; payload = model_job_payload ~recipe ~payload
    ; status = Queued
    ; retry_policy = Never
    ; attempt = 0
    ; created_at = now t
    ; started_at = None
    ; next_run_at = None
    ; completed_at = None
    ; result = None
    ; delivery
    ; launch = None
    ; progress = None
    }
;;

let review_permission t actor_ref profile invocation =
  match !actor_ref with
  | None ->
    Error
      Agent_session.Permission_reviewer.Error.
        { code = "reviewer.unavailable"; message = "session actor is unavailable" }
  | Some actor -> Permission_review_service.review ~now:(now t) ~actor ~profile invocation
;;

let expire_permissions t actor profile ~now =
  match Agent_session.Session_actor.state actor with
  | Error _ -> ()
  | Ok state ->
    List.filter state.permissions ~f:(permission_is_expired now)
    |> List.iter ~f:(fun permission ->
      ignore
        (Permission_review_service.resolve_timeout ~now ~actor ~profile permission
         : (Agent_protocol.Permission.t, Agent_protocol.Error.t) result))
;;

let shell_review_permission t actor_ref profile request =
  match !actor_ref with
  | None -> Error "session actor is unavailable"
  | Some actor ->
    let open Result.Let_syntax in
    let%bind current =
      Agent_session.Session_actor.state actor
      |> Result.map_error ~f:(fun error -> error.message)
    in
    let%bind owner =
      shell_permission_owner current
      |> Result.map_error ~f:(fun error -> error.Agent_protocol.Error.message)
    in
    let invocation = shell_policy_invocation request in
    let permission =
      Agent_protocol.Permission.
        { id = Agent_protocol.Id.Permission.create ()
        ; session_id = current.identity.session_id
        ; generation = current.identity.generation
        ; owner
        ; call_id = request.request.context.request_id
        ; tool_name = invocation.tool_name
        ; runtime_identity = Some invocation.identity_digest
        ; invocation_display = invocation.invocation_display
        ; rationale = request.request.rationale
        ; effects = invocation.effects
        ; choices = [ Approve_once; Deny ]
        ; created_at = now t
        ; expires_at = None
        ; state = Pending
        ; resolution = None
        }
    in
    Agent_session.Session_actor.request_review actor ~permission ~review:(fun () ->
      review_permission t actor_ref profile invocation)
    |> Result.map_error ~f:(fun error -> error.message)
    |> Result.map ~f:(shell_broker_response request)
;;

let shell_policy_response t actor_ref profile request =
  match
    Agent_session.Permission_policy.decide
      profile
      ~responder_available:false
      (shell_policy_invocation request)
  with
  | Allow_now -> Shell_runtime.Approval_broker.Approve_once
  | Deny_now reason -> Deny reason
  | Request_permission -> Deny "unattended shell policy requested human approval"
  | Request_review ->
    (match shell_review_permission t actor_ref profile request with
     | Ok response -> response
     | Error message -> Deny message)
;;

let policy_shell_provider t profile actor_ref =
  let broker_ref = ref None in
  let broker =
    Shell_runtime.Approval_broker.create
      ~on_pending:(fun request ->
        Eio.Fiber.fork ~sw:t.sw (fun () ->
          Option.iter !broker_ref ~f:(fun broker ->
            ignore
              (Shell_runtime.Approval_broker.respond
                 broker
                 ~id:request.id
                 (shell_policy_response t actor_ref profile request)
               : (unit, Shell_runtime.Approval_broker.error) result))))
      ()
  in
  broker_ref := Some broker;
  Shell_runtime.Approval_broker.Callback broker
;;

let shell_approval_provider t profile actor_ref =
  match profile.Agent_session.Permission_policy.tool_default, profile.fallback with
  | Allow, _ -> Shell_runtime.Approval_broker.Assume_approved
  | Deny, _ -> Auto_deny
  | Ask, _ ->
    interactive_shell_provider t profile actor_ref ~review_timeout:(fun request () ->
      review_permission t actor_ref profile (shell_policy_invocation request))
  | Policy, _ -> policy_shell_provider t profile actor_ref
;;

let extension_actor actor_ref =
  Result.of_option
    !actor_ref
    ~error:(unavailable Invalid_state "extension runtime is not installed in a session")
;;

let authorize_extension_native t actor_ref profile native invocation binding =
  let module A = Agent_session.Session_actor in
  let module I = Agent_protocol.Invocation in
  let module Policy = Agent_session.Permission_policy in
  let open Result.Let_syntax in
  let%bind actor = extension_actor actor_ref in
  let%bind state = A.state actor in
  let%bind () =
    match Agent_session.Native_tool_invocation.current_scope () with
    | Active current
      when I.equal_context current.context invocation.I.context
           && Agent_protocol.Id.Session.equal
                current.context.session_id
                state.identity.session_id
           && current.context.generation = state.identity.generation
           && String.equal
                profile.Policy.revision_digest
                state.spec.permission_profile_digest -> Ok ()
    | Active _ | Expired | Unbound ->
      Error
        (unavailable
           Permission_denied
           "native invocation has no current scoped authority")
  in
  if
    Set.mem
      native.Chat_response.Agent_runtime.shell_tool_names
      invocation.context.tool_name
  then Ok ()
  else (
    let identity_digest =
      String.concat
        ~sep:"\000"
        [ "ochat.native-permission.v2"
        ; profile.Policy.revision_digest
        ; invocation.context.tool_name
        ; invocation.context.implementation_revision
        ; Chat_response.Tool_capability.permission_fingerprint binding
        ; Jsonaf.to_string invocation.context.input
        ]
      |> Chatmd_shell_spec.Source_ref.digest
    in
    let request : Policy.invocation =
      { tool_name = invocation.context.tool_name
      ; identity_digest
      ; invocation_display = invocation.context.tool_name ^ "(<redacted>)"
      ; effects = [ "tool_invocation" ]
      }
    in
    let%bind granted =
      A.invocation_granted actor ~tool_name:request.tool_name ~identity_digest
    in
    match granted with
    | true -> Ok ()
    | false ->
      let decision =
        Policy.decide
          profile
          ~responder_available:(shell_responder_available t state)
          request
      in
      let permission choices =
        Agent_protocol.Permission.
          { id = Agent_protocol.Id.Permission.create ()
          ; session_id = state.identity.session_id
          ; generation = state.identity.generation
          ; owner = Invocation invocation.context.id
          ; call_id =
              Option.value
                invocation.context.provider_call_id
                ~default:(Agent_protocol.Id.Invocation.to_string invocation.context.id)
          ; tool_name = request.tool_name
          ; runtime_identity = Some identity_digest
          ; invocation_display = request.invocation_display
          ; rationale = None
          ; effects = request.effects
          ; choices
          ; created_at = now t
          ; expires_at = shell_permission_expiry t profile
          ; state = Pending
          ; resolution = None
          }
      in
      let accept (resolution : Agent_protocol.Permission.resolution) =
        match resolution.choice with
        | Deny ->
          Error (unavailable Permission_denied "native tool invocation was denied")
        | Approve_once | Approve_session | Approve_prefix | Durable_exact -> Ok ()
      in
      (match decision with
       | Allow_now -> Ok ()
       | Deny_now reason -> Error (unavailable Permission_denied reason)
       | Request_permission ->
         let permission =
           permission
             [ Approve_once; Approve_session; Approve_prefix; Durable_exact; Deny ]
         in
         let fallback =
           match profile.fallback with
           | Fallback_allow -> Agent_protocol.Permission.Approve_once
           | Fallback_deny | Fallback_allow_if_policy | Fallback_reviewer _ -> Deny
         in
         let%bind resolution =
           match profile.fallback with
           | Fallback_reviewer _ ->
             A.request_permission_with_review_fallback
               actor
               ~permission
               ~timeout_seconds:(shell_permission_timeout profile)
               ~fallback
               ~review_on_timeout:(fun () ->
                 review_permission t actor_ref profile request)
           | Fallback_allow | Fallback_deny | Fallback_allow_if_policy ->
             A.request_permission
               actor
               ~permission
               ~timeout_seconds:(shell_permission_timeout profile)
               ~fallback
         in
         accept resolution
       | Request_review ->
         let%bind resolution =
           A.request_review
             actor
             ~permission:(permission [ Approve_once; Deny ])
             ~review:(fun () -> review_permission t actor_ref profile request)
         in
         (match resolution.choice with
          | Approve_once | Deny -> accept resolution
          | Approve_session | Approve_prefix | Durable_exact ->
            Error
              (unavailable Permission_denied "reviewer returned an invalid grant scope"))))
;;

let extension_jobs t actor_ref registry =
  let module A = Agent_session.Session_actor in
  let module Jobs = Agent_session.Script_job_service in
  let host : Jobs.host =
    { stage =
        (fun owner request ->
          let open Result.Let_syntax in
          let%bind actor = extension_actor actor_ref in
          let%bind job = A.prepare_background_job_launch actor ~owner request in
          let%bind state = A.state actor in
          let key =
            Job_capacity.Key.create
              ~principal_id:state.identity.creating_principal
              ~prompt:
                (Option.value_map
                   state.spec.prompt_definition_id
                   ~default:"<local>"
                   ~f:Agent_protocol.Id.Prompt_definition.to_string)
              ~workspace_conflict_domain:state.spec.workspace_instance.conflict_domain
              ~session_id:job.session_id
              ~kind:job.kind
              ~nested_depth:(Option.value_exn job.launch).nested_depth
          in
          let%bind reservation = Job_capacity.reserve_job t.job_capacity key ~job in
          let%bind reservation =
            Result.of_option
              reservation
              ~error:(unavailable Resource_limit "background job capacity is exhausted")
          in
          let%map () =
            A.stage_background_job
              actor
              ~job
              ~capacity:
                { publish = (fun () -> Job_capacity.publish reservation)
                ; abort = (fun () -> Job_capacity.abort reservation)
                }
          in
          job)
    ; select =
        (fun owner ids ->
          Result.bind (extension_actor actor_ref) ~f:(fun actor ->
            A.select_background_jobs actor ~owner ~ids))
    ; abort =
        (fun owner id ->
          ignore
            (Result.bind (extension_actor actor_ref) ~f:(fun actor ->
               A.abort_background_job actor ~owner ~id)
             : (unit, Agent_protocol.Error.t) result))
    ; get =
        (fun owner id ->
          let open Result.Let_syntax in
          let%bind actor = extension_actor actor_ref in
          A.read_script_job actor ~owner ~id)
    ; materialize =
        (fun owner expected ->
          Result.bind (extension_actor actor_ref) ~f:(fun actor ->
            A.read_script_job_result actor ~owner ~expected))
    ; cancel =
        (fun owner id ->
          Result.bind (extension_actor actor_ref) ~f:(fun actor ->
            A.cancel_script_job actor ~owner ~id))
    }
  in
  Jobs.create
    ~env:t.env
    ~policy:Chat_response.One_off_request.default_policy
    ~current_capabilities:(fun () -> registry)
    ~host
;;

let extension_subscriptions t actor_ref =
  let module A = Agent_session.Session_actor in
  let module Service = Agent_session.Script_subscription_service in
  let host : Service.host =
    { create =
        (fun owner source ~kind ~lifetime_ms ~wake ~completion_schema ->
          Result.bind (extension_actor actor_ref) ~f:(fun actor ->
            A.create_script_subscription
              actor
              ~owner
              ~source
              ~kind
              ~lifetime_ms
              ~wake
              ~completion_schema))
    ; stage =
        (fun owner source ~previous ~next ->
          Result.bind (extension_actor actor_ref) ~f:(fun actor ->
            A.stage_subscription_mutation actor ~owner ~source ~previous ~next))
    ; get =
        (fun owner source id ->
          Result.bind (extension_actor actor_ref) ~f:(fun actor ->
            A.read_script_subscription actor ~owner ~source ~id))
    ; finish =
        (fun owner source id ~expected_epoch completion ->
          Result.bind (extension_actor actor_ref) ~f:(fun actor ->
            A.finish_script_subscription
              actor
              ~owner
              ~source
              ~id
              ~expected_epoch
              completion))
    ; select =
        (fun owner source receipts ->
          Result.bind (extension_actor actor_ref) ~f:(fun actor ->
            A.select_subscription_mutations actor ~owner ~source ~receipts))
    ; abort =
        (fun owner receipt ->
          ignore
            (Result.bind (extension_actor actor_ref) ~f:(fun actor ->
               A.abort_subscription_mutation actor ~owner ~receipt)
             : (unit, Agent_protocol.Error.t) result))
    ; get_job =
        (fun owner id ->
          Result.bind (extension_actor actor_ref) ~f:(fun actor ->
            A.read_script_job actor ~owner ~id))
    }
  in
  Service.create ~limits:t.limits.subscriptions ~host
;;

let extension_notifications actor_ref =
  let module A = Agent_session.Session_actor in
  let module Service = Agent_session.Script_notification_service in
  let host : Service.host =
    { create =
        (fun owner source ~correlation ~completion ~wake ~disclosure_pins ->
          Result.bind (extension_actor actor_ref) ~f:(fun actor ->
            A.create_script_notification
              ~disclosure_pins
              actor
              ~owner
              ~source
              ~correlation
              ~completion
              ~wake))
    ; get =
        (fun owner source id ->
          Result.bind (extension_actor actor_ref) ~f:(fun actor ->
            A.read_script_notification actor ~owner ~source ~id))
    ; select =
        (fun owner source receipts ->
          Result.bind (extension_actor actor_ref) ~f:(fun actor ->
            A.select_notification_mutations actor ~owner ~source ~receipts))
    ; abort =
        (fun owner receipt ->
          ignore
            (Result.bind (extension_actor actor_ref) ~f:(fun actor ->
               A.abort_notification_mutation actor ~owner ~receipt)
             : (unit, Agent_protocol.Error.t) result))
    }
  in
  Service.create ~host
;;

let extension_ingress actor_ref =
  let module A = Agent_session.Session_actor in
  let module Service = Agent_session.Script_ingress_service in
  let with_actor f = Result.bind (extension_actor actor_ref) ~f in
  let host : Service.host =
    { register =
        (fun owner source ~subscription_id ~expected_epoch ~namespace ~schema ->
          with_actor (fun actor ->
            A.create_script_ingress
              actor
              ~owner
              ~source
              ~subscription_id
              ~expected_epoch
              ~namespace
              ~schema))
    ; get =
        (fun owner source id ->
          with_actor (fun actor -> A.read_script_ingress actor ~owner ~source ~id))
    ; revoke =
        (fun owner source id ~reason ->
          with_actor (fun actor ->
            A.revoke_script_ingress actor ~owner ~source ~id ~reason))
    ; select =
        (fun owner source receipts ->
          with_actor (fun actor ->
            A.select_ingress_mutations actor ~owner ~source ~receipts))
    ; abort =
        (fun owner receipt ->
          ignore
            (with_actor (fun actor -> A.abort_ingress_mutation actor ~owner ~receipt)
             : (unit, Agent_protocol.Error.t) result))
    }
  in
  Service.create ~host
;;

let extension_schedules actor_ref =
  let module A = Agent_session.Session_actor in
  let module Service = Agent_session.Script_schedule_service in
  let host : Service.host =
    { create =
        (fun owner source ~delay_ms ~payload ~misfire ->
          Result.bind (extension_actor actor_ref) ~f:(fun actor ->
            A.create_script_schedule actor ~owner ~source ~delay_ms ~payload ~misfire))
    ; stage =
        (fun owner source ~previous ~next ->
          Result.bind (extension_actor actor_ref) ~f:(fun actor ->
            A.stage_schedule_mutation actor ~owner ~source ~previous ~next))
    ; get =
        (fun owner source id ->
          Result.bind (extension_actor actor_ref) ~f:(fun actor ->
            A.read_script_schedule actor ~owner ~source ~id))
    ; select =
        (fun owner source receipts ->
          Result.bind (extension_actor actor_ref) ~f:(fun actor ->
            A.select_schedule_mutations actor ~owner ~source ~receipts))
    ; abort =
        (fun owner receipt ->
          ignore
            (Result.bind (extension_actor actor_ref) ~f:(fun actor ->
               A.abort_schedule_mutation actor ~owner ~receipt)
             : (unit, Agent_protocol.Error.t) result))
    }
  in
  Service.create ~host
;;

let extension_services t profile actor_ref ~(state : Agent_session.Session_state.t) =
  let module A = Agent_session.Session_actor in
  let notification_snapshot () =
    let open Result.Let_syntax in
    let%bind actor = extension_actor actor_ref in
    let%bind current = A.state actor in
    let%bind () =
      match
        Agent_protocol.Id.Session.equal
          current.identity.session_id
          state.identity.session_id
        && Int.equal current.identity.generation state.identity.generation
        && Agent_protocol.Id.Prompt_revision.equal
             current.spec.prompt_revision_id
             state.spec.prompt_revision_id
        && Agent_protocol.Id.Workspace_instance.equal
             current.spec.workspace_instance.id
             state.spec.workspace_instance.id
        && Agent_session.Workspace_instance.equal_canonical_identity
             current.spec.workspace_instance.canonical_root
             state.spec.workspace_instance.canonical_root
        && Agent_session.Workspace_definition.equal_access
             current.spec.workspace_instance.access
             state.spec.workspace_instance.access
        && String.equal
             current.spec.permission_profile_digest
             state.spec.permission_profile_digest
      with
      | true -> Ok ()
      | false -> Error (unavailable Conflict "notification runtime pin changed")
    in
    Ok (actor, current)
  in
  Agent_session.Runtime_builder.
    { runtime_policy =
        (match state.automatic_turn_budget with
         | Some budget -> budget.policy
         | None -> t.chatml_runtime_policy)
    ; script_tools =
        (fun native ->
          let registry =
            Lazy.force native.Chat_response.Agent_runtime.capabilities
            |> Result.map_error ~f:(fun error ->
              error.Chat_response.Tool_capability.message)
            |> Result.ok_or_failwith
          in
          Agent_session.Script_tool_calls.create
            ~registry:(fun () -> registry)
            ~moderator_names:String.Set.empty
            ~now:(fun () -> now t)
            ~is_halted:(fun () ->
              match Result.bind (extension_actor actor_ref) ~f:A.state with
              | Error _ -> true
              | Ok state ->
                state.halted
                || Option.is_some state.failure
                || Agent_protocol.Session.equal_desired_state
                     state.lifecycle.desired
                     Stopped
                || Option.exists state.active_operation ~f:(fun operation ->
                  match operation.state with
                  | Cancelling | Cancelled | Failed _ | Interrupted _ -> true
                  | Starting | Running | Completed -> false))
            ~requires_active_moderator:(fun _ -> false)
            ~authorize:(authorize_extension_native t actor_ref profile native)
            ~prepare_output:(function
              | Openai.Responses.Tool_output.Output.Text text -> Ok (`String text)
              | output -> Ok (Openai.Responses.Tool_output.Output.jsonaf_of_t output))
            ~defer_observation:(fun _ -> Ok ())
          |> fun tools ->
          Agent_session.Script_tool_calls.with_job_service
            tools
            (extension_jobs t actor_ref registry)
          |> fun tools ->
          Agent_session.Script_tool_calls.with_subscription_service
            tools
            (extension_subscriptions t actor_ref)
          |> fun tools ->
          Agent_session.Script_tool_calls.with_schedule_service
            tools
            (extension_schedules actor_ref)
          |> fun tools ->
          Agent_session.Script_tool_calls.with_notification_service
            tools
            (extension_notifications actor_ref)
          |> fun tools ->
          Agent_session.Script_tool_calls.with_ingress_service
            tools
            (extension_ingress actor_ref)
          |> fun tools ->
          Agent_session.Script_tool_calls.with_progress
            tools
            ~emit:(fun invocation progress ->
              match extension_actor actor_ref with
              | Error _ -> ()
              | Ok actor ->
                A.publish_job_progress actor ~invocation_id:invocation.context.id progress))
    ; standalone_execution_limits =
        Agent_session.Standalone_tool_dispatch.declared_execution_limits
    ; standalone_completion =
        (fun ~tools job ->
          let open Result.Let_syntax in
          let%bind actor, current = notification_snapshot () in
          A.deliver_standalone_completion
            actor
            ~revision:current.counters.revision
            ~job
            ~current_capabilities:
              (Agent_session.Script_tool_calls.current_capabilities tools)
            ~policy:Chat_response.One_off_request.default_policy)
    ; one_off_policy = Chat_response.One_off_request.default_policy
    ; authoring_validation_host = t.authoring_validation_host
    ; claim_lifecycle =
        (fun ~event ~snapshot handle ->
          let open Result.Let_syntax in
          let%bind actor = extension_actor actor_ref in
          A.with_current_moderator_event actor ~operation_id:None ~event ~snapshot handle)
    ; lifecycle_started =
        (fun observer ->
          List.exists state.moderator_executions ~f:(fun receipt ->
            receipt.context.generation = state.identity.generation
            && Agent_protocol.Invocation.equal_observer receipt.context.source observer
            && (match receipt.context.phase with
                | Session_start | Session_resume -> true
                | Turn_start
                | Message_appended
                | Pre_tool_call
                | Post_tool_response
                | Turn_end
                | Internal_event -> false)
            &&
            match receipt.status with
            | Completed _ -> true
            | Running | Failed _ | Interrupted _ -> false))
    ; notification_input =
        (fun ~source ~tools ~operation_id () ->
          let open Result.Let_syntax in
          let%bind actor, current = notification_snapshot () in
          let%bind plan =
            Agent_session.Notification_delivery.prepare_for_runtime
              ~state:current
              ~source
              ~current_capabilities:
                (Agent_session.Script_tool_calls.current_capabilities tools)
              ~policy:Chat_response.One_off_request.default_policy
              ~max_count:t.limits.notifications.max_per_source
          in
          match
            Eio.Cancel.protect (fun () ->
              A.consume_notifications actor ~operation_id plan)
          with
          | Error { code = Conflict; _ } ->
            Ok Chat_response.In_memory_stream.Safe_point_input.empty
          | result -> result)
    ; initial_notification_input =
        (fun ~source ~tools ~operation_id () ->
          let open Result.Let_syntax in
          let%bind actor, current = notification_snapshot () in
          let%bind plan =
            Agent_session.Notification_delivery.prepare_idle_for_runtime
              ~state:current
              ~source
              ~current_capabilities:
                (Agent_session.Script_tool_calls.current_capabilities tools)
              ~policy:Chat_response.One_off_request.default_policy
              ~max_count:t.limits.notifications.max_per_source
          in
          match
            Eio.Cancel.protect (fun () ->
              A.consume_initial_notifications actor ~operation_id plan)
          with
          | Error { code = Conflict; _ } ->
            Ok Chat_response.In_memory_stream.Safe_point_input.empty
          | result -> result)
    ; idle_notifications =
        (fun ~source ~tools () ->
          let open Result.Let_syntax in
          let%bind actor, current = notification_snapshot () in
          let%bind plan =
            Agent_session.Notification_delivery.prepare_idle_for_runtime
              ~state:current
              ~source
              ~current_capabilities:
                (Agent_session.Script_tool_calls.current_capabilities tools)
              ~policy:Chat_response.One_off_request.default_policy
              ~max_count:t.limits.notifications.max_per_source
          in
          match
            Eio.Cancel.protect (fun () -> A.deliver_idle_notifications actor plan)
          with
          | Error { code = Conflict; _ } -> Ok false
          | result -> result)
    ; history =
        (fun () ->
          let result =
            let open Result.Let_syntax in
            let%bind actor = extension_actor actor_ref in
            let%bind state = A.state actor in
            Agent_session.History_codec.all_of_protocol
              state.conversation.canonical_history
          in
          Result.map_error result ~f:(fun error -> error.Agent_protocol.Error.message)
          |> Result.ok_or_failwith)
    }
;;

let add_bound_job actor_ref pending job =
  match !actor_ref with
  | Some actor ->
    Agent_session.Session_actor.add_job actor job
    |> Result.map ~f:(fun job -> Agent_protocol.Id.Job.to_string job.id)
    |> Result.map_error ~f:(fun error -> error.message)
  | None ->
    pending := !pending @ [ job ];
    Ok (Agent_protocol.Id.Job.to_string job.id)
;;

let rec await_call_cancellation t actor (job : Agent_protocol.Job.t) =
  let state = Agent_session.Session_actor.state actor in
  let is_active =
    Option.value_map (Result.ok state) ~default:false ~f:(fun state ->
      state.identity.generation = job.generation
      && (not state.halted)
      && List.exists state.jobs ~f:(fun current ->
        Agent_protocol.Id.Job.compare current.id job.id = 0
        &&
        match current.status with
        | Running -> true
        | _ -> false))
  in
  if is_active
  then (
    Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.01;
    await_call_cancellation t actor job)
  else ()
;;

let cancelled_call actor (job : Agent_protocol.Job.t) =
  Eio.Cancel.protect (fun () ->
    ignore
      (Agent_session.Session_actor.interrupt_job
         actor
         ~job_id:job.id
         ~generation:job.generation
         ~attempt:job.attempt
         ~reason:"synchronous model call was cancelled"
       : (Agent_protocol.Job.t, Agent_protocol.Error.t) result));
  Ok
    (Chat_response.Moderation.Capabilities.Model_error
       "synchronous model call was cancelled")
;;

let job_services t state actor_ref pending =
  let spawn_model ~recipe ~payload =
    let job = model_job t state ~recipe ~payload ~delivery:Pending in
    add_bound_job actor_ref pending job
  in
  let capacity_key () =
    let prompt =
      Option.value_map
        state.spec.prompt_definition_id
        ~default:"<local>"
        ~f:Agent_protocol.Id.Prompt_definition.to_string
    in
    Job_capacity.Key.create
      ~principal_id:state.identity.creating_principal
      ~prompt
      ~workspace_conflict_domain:state.spec.workspace_instance.conflict_domain
      ~session_id:state.identity.session_id
      ~kind:Agent_protocol.Job.Model_call
      ~nested_depth:0
  in
  let complete_call actor job result =
    let outcome =
      match result with
      | Ok (Chat_response.Moderation.Capabilities.Model_ok payload) ->
        Agent_session.Runtime_builder.Model_succeeded payload
      | Ok (Model_refused message) | Ok (Model_error message) | Error message ->
        Model_failed message
    in
    Agent_session.Session_actor.complete_job
      actor
      ~job_id:job.Agent_protocol.Job.id
      ~generation:job.generation
      ~attempt:job.attempt
      outcome
    |> Result.map_error ~f:(fun error -> error.message)
    |> Result.bind ~f:(fun _ -> result)
  in
  let run_call actor job execute =
    match
      Eio.Fiber.first
        (fun () -> `Completed (execute ()))
        (fun () ->
           await_call_cancellation t actor job;
           `Cancelled)
    with
    | `Completed result -> complete_call actor job result
    | `Cancelled -> cancelled_call actor job
    | exception (Eio.Cancel.Cancelled _ as exn) ->
      Eio.Cancel.protect (fun () ->
        ignore
          (Agent_session.Session_actor.interrupt_job
             actor
             ~job_id:job.Agent_protocol.Job.id
             ~generation:job.generation
             ~attempt:job.attempt
             ~reason:"synchronous model call was cancelled"
           : (Agent_protocol.Job.t, Agent_protocol.Error.t) result));
      raise exn
    | exception exn ->
      complete_call
        actor
        job
        (Error ("synchronous model call failed: " ^ Exn.to_string exn))
  in
  let call_model ~recipe ~payload ~execute =
    match !actor_ref with
    | None -> Error "synchronous model call requires an initialized session actor"
    | Some actor ->
      (match Job_capacity.try_acquire t.job_capacity (capacity_key ()) with
       | Error error -> Error error.message
       | Ok None -> Error "synchronous model call capacity is currently unavailable"
       | Ok (Some lease) ->
         Exn.protect
           ~finally:(fun () -> Job_capacity.release lease)
           ~f:(fun () ->
             let open Result.Let_syntax in
             let job = model_job t state ~recipe ~payload ~delivery:Not_required in
             let%bind job =
               Agent_session.Session_actor.add_job actor job
               |> Result.map_error ~f:(fun error -> error.message)
             in
             let%bind claimed =
               Agent_session.Session_actor.claim_job
                 actor
                 ~job_id:job.id
                 ~generation:job.generation
               |> Result.map_error ~f:(fun error -> error.message)
             in
             match claimed with
             | None -> Error "synchronous model call could not claim its durable job"
             | Some job -> run_call actor job execute))
  in
  Agent_session.Runtime_builder.{ spawn_model; call_model }
;;

let flush_pending_jobs actor jobs =
  List.fold_result jobs ~init:() ~f:(fun () job ->
    Agent_session.Session_actor.add_job actor job |> Result.map ~f:(fun _ -> ()))
;;

let install_moderator_if_changed actor moderator_snapshot =
  let open Result.Let_syntax in
  let%bind state = Agent_session.Session_actor.state actor in
  if Option.equal Jsonaf.exactly_equal state.moderator moderator_snapshot
  then Ok ()
  else
    Agent_session.Session_actor.change_moderator actor moderator_snapshot
    |> Result.map ~f:(fun _ -> ())
;;

let prepare_extension_runtime_scope t build =
  let ready, ready_u = Eio.Promise.create () in
  let stop, stop_u = Eio.Promise.create () in
  let exited, exited_u = Eio.Promise.create () in
  let stopping = Atomic.make false in
  let delivered = ref false in
  let signal_stop () =
    if not (Atomic.exchange stopping true) then Eio.Promise.resolve stop_u ()
  in
  let stop_and_join () =
    Eio.Cancel.protect (fun () ->
      signal_stop ();
      Eio.Promise.await exited)
  in
  let deliver result =
    delivered := true;
    Eio.Promise.resolve ready_u result
  in
  Eio.Fiber.fork ~sw:t.sw (fun () ->
    Exn.protect
      ~finally:(fun () -> Eio.Promise.resolve exited_u ())
      ~f:(fun () ->
        try
          Eio.Cancel.sub (fun context ->
            Eio.Switch.run (fun sw ->
              Eio.Fiber.fork_daemon ~sw (fun () ->
                Eio.Promise.await stop;
                Eio.Cancel.cancel context Exit;
                `Stop_daemon);
              match build sw with
              | Error error ->
                deliver (Ok (Error error));
                signal_stop ();
                Eio.Promise.await stop
              | Ok runtime ->
                let close_started = Atomic.make false in
                let close () =
                  Eio.Cancel.protect (fun () ->
                    if Atomic.exchange close_started true
                    then Eio.Promise.await exited
                    else
                      Exn.protect
                        ~f:runtime.Agent_session.Runtime_builder.close
                        ~finally:stop_and_join)
                in
                deliver (Ok (Ok { runtime with close }));
                Eio.Promise.await stop))
        with
        | exn ->
          let backtrace = Stdlib.Printexc.get_raw_backtrace () in
          (match !delivered, Atomic.get stopping with
           | false, _ -> deliver (Error (exn, backtrace))
           | true, true -> ()
           | true, false -> Exn.raise_with_original_backtrace exn backtrace)));
  match Eio.Promise.await ready with
  | Ok (Ok runtime) -> Ok runtime
  | Ok (Error error) ->
    stop_and_join ();
    Error error
  | Error (exn, backtrace) ->
    stop_and_join ();
    Exn.raise_with_original_backtrace exn backtrace
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    stop_and_join ();
    Exn.raise_with_original_backtrace exn backtrace
;;

let prepare_runtime_at_paths
      t
      paths
      storage_paths
      source
      profile
      (state : Agent_session.Session_state.t)
      ~next_history_sequence
      ~existing_history
      ~existing_moderator_snapshot
      ~actor_ref
      ~pending_schedule_operations
      ~pending_jobs
  =
  let open Result.Let_syntax in
  let shell_state = ref state.Agent_session.Session_state.shell in
  let approval_provider = shell_approval_provider t profile actor_ref in
  let approval_store = shell_approval_store state actor_ref shell_state in
  let construct sw build manifest_authorizer =
    build
      ~sw
      ~env:t.env
      ~paths
      ~storage_paths
      ~session_id:state.Agent_session.Session_state.identity.session_id
      ~history_namespace:(Agent_protocol.Id.Session.to_string state.identity.session_id)
      ~next_history_sequence
      ~existing_history
      ~existing_moderator_snapshot
      ~moderator_reservation_size:t.limits.moderator_reservation_size
      ~manifest_authorizer
      ~approval_provider
      ~approval_store
      ~permission_profile:profile
      ~model_post_stream:t.model_post_stream
      ~review_permission:(review_permission t actor_ref profile)
      ~schedule_services:(schedule_services t state actor_ref pending_schedule_operations)
      ~job_services:(job_services t state actor_ref pending_jobs)
  in
  let%map runtime =
    match source with
    | Authored revision ->
      let build =
        match t.qualify_chatml_extensions with
        | false -> Agent_session.Runtime_builder.build ~revision
        | true ->
          Agent_session.Runtime_builder.build_with_extensions
            ~revision
            ~services:(extension_services t profile actor_ref ~state)
      in
      let construct sw =
        construct
          sw
          build
          (manifest_authorizer t profile revision state actor_ref shell_state)
      in
      (match t.qualify_chatml_extensions with
       | false -> construct t.sw
       | true -> prepare_extension_runtime_scope t construct)
    | Generated_artifact { artifact; _ } ->
      let%bind parent, record = generated_parent t state in
      let parent_stop_epoch =
        Option.value
          state.parent_stop_epoch
          ~default:(Option.value record.admission.parent_stop_epoch ~default:0L)
      in
      let%bind artifact_store =
        Agent_store.Prompt_artifact_store.create
          ~env:t.env
          ~root:
            (Agent_store.Data_root.prompt_artifacts_path
               (Agent_store.Session_store.data_root t.store))
        |> Result.map_error ~f:protocol_of_store
      in
      let reference = Agent_store.Delegation_store.reference record in
      let capabilities (runtime : Agent_session.Runtime_builder.t) =
        match runtime.native_runtime with
        | None ->
          Error
            (unavailable
               Invalid_state
               "delegation.runtime_unavailable: parent does not expose qualified native \
                resources")
        | Some native ->
          Lazy.force native.capabilities
          |> Result.map_error ~f:(fun error ->
            unavailable Permission_denied error.Chat_response.Tool_capability.message)
      in
      Delegated_runtime.prepare
        ~sw:t.sw
        ~parent:parent.runtime
        ~on_revoked:(fun () ->
          match Agent_session.Session_actor.state parent.actor, !actor_ref with
          | Ok current, Some actor
            when Agent_protocol.Session.equal_desired_state
                   current.lifecycle.desired
                   Stopped
                 || not (Int64.equal current.stop_epoch parent_stop_epoch) ->
            Agent_session.Session_actor.stop_delegated_at_epoch
              actor
              ~reference
              ~epoch:current.stop_epoch
            |> Result.map ~f:ignore
          | Error { code = Server_shutting_down; _ }, _ | Ok _, _ -> Ok ()
          | Error error, _ -> Error error)
        ~build:(fun ~sw parent_runtime ->
          let%bind current = capabilities parent_runtime in
          let%bind native =
            Result.of_option
              parent_runtime.native_runtime
              ~error:
                (unavailable
                   Invalid_state
                   "delegation.runtime_unavailable: parent native runtime is missing")
          in
          let%bind definition =
            Agent_session.Generated_definition.restore
              ~env:t.env
              ~artifact_store
              ~revision_id:artifact.revision_id
              ~manifest_sha256:record.admission.manifest_sha256
              ~current_capabilities:(fun () -> current)
              ~pins:record.admission.capability_pins
              ()
            |> Result.map_error ~f:(fun diagnostics ->
              unavailable
                Prompt_unavailable
                (List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string
                 |> String.concat ~sep:"\n"))
          in
          let lookup id =
            Session_registry.find t.registry id
            |> Result.of_option
                 ~error:
                   (unavailable
                      Permission_denied
                      "delegation.parent_missing: ancestor is not loaded")
          in
          let authority =
            Agent_session.Delegation_authority.create
              ~max_depth:t.limits.delegation_max_depth
              ~parent_stop_epoch
              ~reference
              ~capabilities:
                (Chat_response.Generated_admission.capabilities
                   (Agent_session.Generated_definition.admission definition))
              ~host:
                { state =
                    (fun id ->
                      Result.bind (lookup id) ~f:(fun entry ->
                        Agent_session.Session_actor.state entry.actor))
                ; resolve =
                    (fun reference ->
                      Agent_store.Delegation_store.resolve
                        (Agent_store.Session_store.delegations t.store)
                        reference
                      |> Result.map_error ~f:protocol_of_store)
                ; capabilities =
                    (fun id ->
                      if Agent_protocol.Id.Session.equal id record.key.parent_session_id
                      then Ok current
                      else
                        Result.bind (lookup id) ~f:(fun entry ->
                          Runtime_owner.with_background_runtime entry.runtime capabilities))
                }
              ()
          in
          construct
            sw
            (Agent_session.Runtime_builder.build_generated
               ~services:(extension_services t profile actor_ref ~state)
               ~definition
               ~artifact_store
               ~parent_runtime:native
               ~authority)
            (fun _ ->
               Shell_runtime.Manifest_authorizer.Reject
                 "generated definitions cannot authorize new shell manifests"))
  in
  runtime, shell_state
;;

let prepare_runtime
      t
      handle
      revision
      profile
      state
      ~next_history_sequence
      ~existing_history
      ~existing_moderator_snapshot
      ~actor_ref
      ~pending_schedule_operations
      ~pending_jobs
  =
  let open Result.Let_syntax in
  let%bind paths = runtime_paths t handle revision state in
  prepare_runtime_at_paths
    t
    paths
    paths
    revision
    profile
    state
    ~next_history_sequence
    ~existing_history
    ~existing_moderator_snapshot
    ~actor_ref
    ~pending_schedule_operations
    ~pending_jobs
  |> Result.map ~f:(fun (runtime, shell) -> runtime, !shell)
;;

let install_runtime_state state runtime shell =
  let desired = state.Agent_session.Session_state.lifecycle.desired in
  let observed =
    match desired with
    | Agent_protocol.Session.Running -> Agent_protocol.Session.Idle
    | Stopped -> Stopped
  in
  { state with
    lifecycle = { desired; observed }
  ; conversation =
      { state.conversation with
        canonical_history =
          Agent_session.History_codec.all_to_protocol
            ~previous:state.conversation.canonical_history
            runtime.Agent_session.Runtime_builder.initial_history
      ; initial_prompt_entry_count = runtime.initial_prompt_entry_count
      ; next_history_sequence = Int64.of_int runtime.reserved_history_through
      ; reserved_history_through = Int64.of_int runtime.reserved_history_through
      }
  ; moderator = runtime.moderator_snapshot
  ; shell
  }
;;

let runnable_job job =
  match job.Agent_protocol.Job.status with
  | Queued -> true
  | Running
  | Waiting_permission _
  | Waiting_completion _
  | Succeeded
  | Failed _
  | Cancelled
  | Interrupted _ -> false
;;

let deliverable_job job =
  match job.Agent_protocol.Job.delivery with
  | Pending -> true
  | Not_required | Delivered _ | Discarded _ -> false
;;

let active_schedule schedule =
  match schedule.Agent_protocol.Schedule.status with
  | Scheduled | Delivering -> true
  | Delivered | Cancelled | Failed _ -> false
;;

let owner_grace_deadline state =
  List.find_map state.Agent_session.Session_state.attachments ~f:(fun attachment ->
    match attachment.Agent_protocol.Session.Attachment.mode, attachment.owner_lease with
    | Owner_read_write, Some lease -> lease.disconnect_grace_until
    | (Owner_read_write | Read_write | Read_only), _ -> None)
;;

let index_entry state =
  let earliest_schedule_due =
    state.Agent_session.Session_state.schedules
    |> List.filter ~f:active_schedule
    |> List.map ~f:(fun schedule -> schedule.Agent_protocol.Schedule.next_due_at)
    |> List.min_elt ~compare:Agent_protocol.Timestamp.compare
  in
  Agent_store.Session_index.Entry.
    { session = Agent_session.Session_state.summary state
    ; runnable_job_count = List.count state.jobs ~f:runnable_job
    ; deliverable_job_count = List.count state.jobs ~f:deliverable_job
    ; earliest_schedule_due
    ; owner_grace_deadline = owner_grace_deadline state
    ; pending_initial_start = state.pending_initial_start
    ; archived = false
    }
;;

let persist_metadata t handle state =
  let open Result.Let_syntax in
  let%bind () =
    Agent_store.Session_store.write_metadata t.store handle (metadata state)
  in
  Agent_store.Session_index.upsert
    (Agent_store.Session_store.session_index t.store)
    (index_entry state)
;;

let create_journal t handle session_id =
  let open Result.Let_syntax in
  let%bind journal =
    Agent_store.Journal.create
      ~env:t.env
      ~directory:(Agent_store.Session_store.Handle.journal_directory handle)
      ~max_payload_length:t.limits.max_journal_payload
      ~max_segment_bytes:t.limits.max_segment_bytes
      ~max_segment_frames:t.limits.max_segment_frames
  in
  let%map writer =
    Agent_store.Commit_writer.create
      ~sw:t.sw
      ~journal
      ~session_id
      ~next_transaction_sequence:1L
      ~previous_transaction_hash:None
      ~queue_capacity:t.limits.commit_queue_capacity
  in
  journal, writer
;;

let mark_command_accepted t encoded transaction_sequence =
  let open Result.Let_syntax in
  let result =
    let%bind audit = Agent_store.Idempotency_store.Command_audit.decode encoded in
    Agent_store.Idempotency_store.mark_accepted
      t.idempotency_store
      ~key:audit.key
      ~request_digest:audit.request_digest
      ~transaction_sequence
    |> Result.map ~f:(fun _ -> ())
  in
  ignore (result : (unit, Agent_store.Store_error.t) result)
;;

let workspace_lease_mode = function
  | Agent_session.Workspace_definition.Read_only | Shared_write ->
    Some Agent_session.Workspace_lease.Shared
  | Exclusive -> Some Exclusive
;;

let quota_overflow = function
  | Agent_session.Workspace_definition.Reject -> Agent_session.Quota_manager.Reject
  | Queue -> Queue
;;

let capacity_for_state t state =
  let open Result.Let_syntax in
  match state.Agent_session.Session_state.spec.quota_key with
  | None -> Ok None
  | Some quota_key ->
    let%bind principal_id =
      state.identity.creating_principal
      |> Result.of_option
           ~error:(unavailable Invalid_state "durable session has no creating principal")
    in
    let%bind definition_id =
      state.spec.workspace_instance.definition_id
      |> Result.of_option
           ~error:
             (unavailable Invalid_state "durable session has no workspace definition")
    in
    let%bind workspace =
      Agent_session.Workspace_catalog.find t.workspaces definition_id
      |> Result.of_option
           ~error:(unavailable Workspace_not_found "workspace definition is unavailable")
    in
    let prompt_limit =
      Agent_session.Workspace_definition.prompt_limit workspace quota_key.prompt_id
    in
    let limit =
      Option.value_map prompt_limit ~default:Int.max_value ~f:(fun value ->
        value.max_root_agents)
    in
    let overflow =
      Option.value_map
        prompt_limit
        ~default:Agent_session.Quota_manager.Reject
        ~f:(fun value -> quota_overflow value.overflow)
    in
    Ok
      (Some
         (Session_capacity.create
            ~manager:t.quota_manager
            ~session_id:state.identity.session_id
            ~principal_id
            ~quota_key
            ~prompt_limit:limit
            ~configured_overflow:overflow
            ~workspace_lease_mode:(workspace_lease_mode workspace.access)))
;;

let event_stops_session (event : Agent_protocol.Event.Durable.t) =
  match Agent_protocol.Event.Durable.Payload.of_json ~kind:event.kind event.payload with
  | Ok (Session_state_changed { observed_state = Stopped; _ }) -> true
  | Ok _ | Error _ -> false
;;

let release_stopped_capacity capacity events =
  if List.exists events ~f:event_stops_session
  then Option.iter capacity ~f:Session_capacity.release
;;

let unload_stopped_runtime t runtime_owner state events =
  match
    ( state.Agent_session.Session_state.lifecycle.observed
    , List.exists events ~f:event_stops_session
    , !runtime_owner )
  with
  | Agent_protocol.Session.Stopped, true, Some runtime ->
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      ignore
        (Runtime_owner.unload_and_wait runtime : (unit, Agent_protocol.Error.t) result))
  | ( ( Queued_for_slot
      | Starting
      | Recovering
      | Idle
      | Running_turn _
      | Compacting _
      | Waiting_for_permission _
      | Stopping
      | Failed _ )
    , _
    , _ )
  | Stopped, false, _
  | Stopped, true, None -> ()
;;

let prune_snapshot t handle journal _installed =
  let open Result.Let_syntax in
  let directory = Agent_store.Session_store.Handle.snapshot_directory handle in
  let%bind _ = Agent_store.Snapshot.prune_older ~env:t.env ~directory ~keep:2 in
  let%bind transaction_sequence =
    Agent_store.Snapshot.retention_floor
      ~env:t.env
      ~directory
      ~max_payload_length:t.limits.snapshot_payload_limit
  in
  let%bind _ =
    Agent_store.Journal.prune_before_transaction journal ~transaction_sequence
  in
  Agent_store.Journal.seal_checkpoint journal
;;

let actor_services
      t
      handle
      journal
      persistence
      durable_events
      capacity
      runtime_owner
      ~creating_principal
  =
  let open Result.Let_syntax in
  let%bind result_blobs =
    Agent_store.Blob_store.with_max_upload_bytes
      t.blob_store
      ~max_upload_bytes:(Int64.of_int t.limits.job_result_max_bytes)
    |> Result.map_error ~f:protocol_of_store
  in
  let principal =
    Option.value_or_thunk creating_principal ~default:Agent_protocol.Id.Principal.create
  in
  let%map job_results =
    Agent_store.Job_result_store.Publisher.create
      ~env:t.env
      ~blobs:result_blobs
      ~sw:t.sw
      ~session:handle
      ~principal
      ~inline_bytes:t.limits.job_result_inline_bytes
      ~max_bytes:t.limits.job_result_max_bytes
  in
  let last_snapshot_sequence = ref 0L in
  let last_snapshot_at = ref (Eio.Time.now (Eio.Stdenv.clock t.env)) in
  let snapshot_due state =
    let events_due =
      Int64.(
        state.Agent_session.Session_state.counters.event_sequence
        - !last_snapshot_sequence
        >= of_int t.limits.snapshot_every_events)
    in
    let elapsed_ms =
      (Eio.Time.now (Eio.Stdenv.clock t.env) -. !last_snapshot_at) *. 1000.
    in
    events_due || Float.(elapsed_ms >= of_int t.limits.snapshot_every_ms)
  in
  let install_snapshot state =
    match
      Agent_session.Session_persistence.install_snapshot
        ~env:t.env
        ~handle
        ~max_payload_length:t.limits.snapshot_payload_limit
        ~transaction_hash:(Agent_session.Session_persistence.transaction_hash persistence)
        state
    with
    | Error _ -> ()
    | Ok installed ->
      ignore
        (prune_snapshot t handle journal installed
         : (unit, Agent_store.Store_error.t) result);
      last_snapshot_sequence := state.counters.event_sequence;
      last_snapshot_at := Eio.Time.now (Eio.Stdenv.clock t.env)
  in
  Agent_session.Session_actor.
    { now = (fun () -> now t)
    ; monotonic_now = (fun () -> Eio.Time.Mono.now (Eio.Stdenv.mono_clock t.env))
    ; job_results = Some job_results
    ; subscription_limits = t.limits.subscriptions
    ; schedule_limits = t.limits.schedules
    ; notification_limits = t.limits.notifications
    ; ingress_limits = t.limits.ingress
    ; create_attachment_id = Agent_protocol.Id.Attachment.create
    ; create_reclaim_token =
        (fun () ->
          let bytes = Cstruct.create 32 in
          Eio.Flow.read_exact (Eio.Stdenv.secure_random t.env) bytes;
          Cstruct.to_string bytes
          |> Base64.encode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet)
    ; state_committed =
        (fun state events ->
          Agent_session.Durable_event_log.append durable_events events;
          release_stopped_capacity capacity events;
          unload_stopped_runtime t runtime_owner state events;
          ignore
            (persist_metadata t handle state : (unit, Agent_store.Store_error.t) result);
          if snapshot_due state then install_snapshot state)
    }
;;

let history_source t actor session_id =
  Agent_session.History_id_source.create
    ~namespace:(Agent_protocol.Id.Session.to_string session_id)
    ~block_size:t.limits.history_block_size
    ~reserve:(fun ~count ->
      Agent_session.Session_actor.reserve_history_block actor ~count)
;;

let restore_state_source t (state : Agent_session.Session_state.t) =
  let open Result.Let_syntax in
  let%bind () = Agent_session.Session_state.validate state in
  match state.spec.protocol.prompt, state.spec.delegation with
  | Generated revision_id, Some reference ->
    let%bind record =
      Agent_store.Delegation_store.resolve
        (Agent_store.Session_store.delegations t.store)
        reference
      |> Result.map_error ~f:protocol_of_store
    in
    let%bind artifact_store =
      Agent_store.Prompt_artifact_store.create
        ~env:t.env
        ~root:
          (Agent_store.Data_root.prompt_artifacts_path
             (Agent_store.Session_store.data_root t.store))
      |> Result.map_error ~f:protocol_of_store
    in
    let%map artifact =
      Agent_session.Generated_definition.load_artifact
        ~artifact_store
        ~revision_id
        ~manifest_sha256:record.admission.manifest_sha256
      |> Result.map_error ~f:(fun diagnostics ->
        unavailable
          Prompt_unavailable
          (List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string
           |> String.concat ~sep:"\n"))
    in
    Generated_artifact
      { artifact
      ; materialized_tree =
          Agent_store.Prompt_artifact_store.materialized_tree artifact_store revision_id
      }
  | _ ->
    (match state.spec.prompt_definition_id with
     | None ->
       Error (unavailable Prompt_unavailable "session has no catalog prompt identity")
     | Some definition_id ->
       Agent_session.Prompt_catalog.restore_revision
         t.prompts
         ~definition_id
         ~revision_id:state.spec.prompt_revision_id
       |> Result.map ~f:(fun revision -> Authored revision)
       |> Result.map_error ~f:(fun diagnostics ->
         unavailable
           Prompt_unavailable
           (List.map diagnostics ~f:(fun value -> value.message)
            |> String.concat ~sep:"\n")))
;;

let restore_state_profile t (state : Agent_session.Session_state.t) =
  permission_profile_revision t state.spec.permission_profile_digest
;;

let prepared_schedules initial operations =
  List.fold operations ~init:initial ~f:(fun schedules -> function
    | Add schedule -> schedule :: schedules
    | Cancel id ->
      List.map schedules ~f:(fun schedule ->
        if Agent_protocol.Id.Schedule.compare schedule.Agent_protocol.Schedule.id id = 0
        then { schedule with status = Cancelled }
        else schedule))
;;

let prepared_state state runtime shell schedules jobs ~fresh_history =
  let conversation = state.Agent_session.Session_state.conversation in
  { state with
    conversation =
      { conversation with
        canonical_history =
          Agent_session.History_codec.all_to_protocol
            ~previous:(if fresh_history then [] else conversation.canonical_history)
            runtime.Agent_session.Runtime_builder.initial_history
      ; initial_prompt_entry_count =
          (if fresh_history
           then runtime.initial_prompt_entry_count
           else conversation.initial_prompt_entry_count)
      ; next_history_sequence = Int64.of_int runtime.reserved_history_through
      ; reserved_history_through = Int64.of_int runtime.reserved_history_through
      }
  ; moderator = runtime.moderator_snapshot
  ; shell = !shell
  ; schedules = prepared_schedules state.schedules !schedules
  ; jobs = state.jobs @ !jobs
  }
;;

let preparation_sequence t state =
  let conversation = state.Agent_session.Session_state.conversation in
  let next =
    Int64.max conversation.next_history_sequence conversation.reserved_history_through
  in
  let maximum = Int.max_value - t.limits.moderator_reservation_size in
  if Int64.(next > of_int maximum)
  then Error (unavailable Invalid_state "history sequence space is exhausted")
  else Ok (Int64.to_int_exn next)
;;

let prepare_detached_runtime t paths storage_paths revision profile state ~fresh_history =
  let open Result.Let_syntax in
  let%bind next_history_sequence = preparation_sequence t state in
  let%bind existing_history =
    if fresh_history
    then Ok None
    else
      Result.map
        (Agent_session.History_codec.all_of_protocol state.conversation.canonical_history)
        ~f:Option.some
  in
  let schedules = ref [] in
  let jobs = ref [] in
  let%bind runtime, shell =
    prepare_runtime_at_paths
      t
      paths
      storage_paths
      revision
      profile
      state
      ~next_history_sequence
      ~existing_history
      ~existing_moderator_snapshot:None
      ~actor_ref:(ref None)
      ~pending_schedule_operations:schedules
      ~pending_jobs:jobs
  in
  Exn.protect
    ~finally:(fun () -> Eio.Cancel.protect runtime.close)
    ~f:(fun () ->
      let%bind _ = runtime.start_moderator () in
      let candidate = prepared_state state runtime shell schedules jobs ~fresh_history in
      let%map () = Agent_session.Session_state.validate candidate in
      candidate)
;;

let with_preparation_storage paths f =
  let name =
    "admin-preparation-" ^ Agent_protocol.Id.Transaction.(create () |> to_string)
  in
  let root = Eio.Path.(paths.Agent_session.Runtime_paths.session_dir / name) in
  Eio.Path.mkdir ~perm:0o700 root;
  Exn.protect
    ~finally:(fun () ->
      Eio.Cancel.protect (fun () -> Eio.Path.rmtree ~missing_ok:true root))
    ~f:(fun () ->
      let cache_dir = Eio.Path.(root / "cache") in
      Eio.Path.mkdir ~perm:0o700 cache_dir;
      f { paths with session_dir = root; cache_dir })
;;

let with_preparation_switch f =
  let ready, resolver = Eio.Promise.create () in
  Eio.Fiber.first
    (fun () ->
       Eio.Switch.run (fun sw ->
         let result = f sw in
         Eio.Promise.resolve resolver result;
         Eio.Fiber.await_cancel ()))
    (fun () -> Eio.Promise.await ready)
;;

let prepare_administration t entry state ~fresh_history =
  try
    let open Result.Let_syntax in
    let%bind handle =
      entry.Session_registry.store_handle
      |> Result.of_option
           ~error:(unavailable Invalid_state "session has no administration store")
    in
    let%bind revision = restore_state_source t state in
    let%bind () = check_source_for_execution t state revision in
    let%bind profile = restore_state_profile t state in
    let%bind paths = runtime_paths t handle revision state in
    with_preparation_storage paths (fun storage_paths ->
      with_preparation_switch (fun sw ->
        prepare_detached_runtime
          { t with sw }
          paths
          storage_paths
          revision
          profile
          state
          ~fresh_history))
  with
  | Eio.Cancel.Cancelled _ as exn -> raise exn
  | exn ->
    Error
      (unavailable
         Invalid_state
         ("administrative preparation failed: " ^ Exn.to_string exn))
;;

let checkpoint_entry t handle journal persistence actor =
  Agent_session.Session_actor.checkpoint actor ~persist:(fun state ->
    let open Result.Let_syntax in
    let%bind installed =
      Agent_session.Session_persistence.install_snapshot
        ~env:t.env
        ~handle
        ~max_payload_length:t.limits.snapshot_payload_limit
        ~transaction_hash:(Agent_session.Session_persistence.transaction_hash persistence)
        state
      |> Result.map_error ~f:protocol_of_store
    in
    prune_snapshot t handle journal installed |> Result.map_error ~f:protocol_of_store)
;;

let close_entry t handle journal persistence runtime writer actor capacity =
  Exn.protect
    ~f:(fun () ->
      Runtime_owner.close_and_wait runtime;
      ignore
        (checkpoint_entry t handle journal persistence actor
         : (unit, Agent_protocol.Error.t) result))
    ~finally:(fun () ->
      Eio.Cancel.protect (fun () ->
        Job_capacity.close_session
          t.job_capacity
          ~session_id:(Agent_store.Session_store.Handle.session_id handle);
        Agent_session.Session_actor.shutdown actor;
        Option.iter capacity ~f:Session_capacity.release;
        Agent_store.Commit_writer.close writer;
        ignore
          (Agent_store.Session_store.close_session t.store handle
           : (unit, Agent_store.Store_error.t) result)))
;;

let close_unregistered_entry t handle runtime writer actor capacity =
  Runtime_owner.close_and_wait runtime;
  Job_capacity.close_session
    t.job_capacity
    ~session_id:(Agent_store.Session_store.Handle.session_id handle);
  Agent_session.Session_actor.shutdown actor;
  Option.iter capacity ~f:Session_capacity.release;
  Agent_store.Commit_writer.close writer
;;

let close_unregistered_runtime t handle runtime writer actor capacity =
  Agent_session.Session_actor.shutdown actor;
  Option.iter capacity ~f:Session_capacity.release;
  runtime.Agent_session.Runtime_builder.close ();
  Job_capacity.close_session
    t.job_capacity
    ~session_id:(Agent_store.Session_store.Handle.session_id handle);
  Agent_store.Commit_writer.close writer
;;

let build_runtime_for_actor t handle actor =
  let open Result.Let_syntax in
  let%bind state = Agent_session.Session_actor.state actor in
  let%bind revision = restore_state_source t state in
  let%bind () = check_source_for_execution t state revision in
  let%bind profile = restore_state_profile t state in
  let%bind reservation =
    Agent_session.Session_actor.reserve_history_block
      actor
      ~count:t.limits.moderator_reservation_size
  in
  let%bind next_history_sequence =
    if Int64.(reservation.first_sequence > of_int Int.max_value)
    then Error (unavailable Invalid_state "history sequence space is exhausted")
    else Ok (Int64.to_int_exn reservation.first_sequence)
  in
  let%bind state = Agent_session.Session_actor.state actor in
  let%bind history =
    Agent_session.History_codec.all_of_protocol state.conversation.canonical_history
  in
  let actor_ref = ref (Some actor) in
  let pending_schedule_operations = ref [] in
  let pending_jobs = ref [] in
  prepare_runtime
    t
    handle
    revision
    profile
    state
    ~next_history_sequence
    ~existing_history:(Some history)
    ~existing_moderator_snapshot:state.moderator
    ~actor_ref
    ~pending_schedule_operations
    ~pending_jobs
  |> Result.bind ~f:(fun (runtime, _shell) ->
    let prepared = ref false in
    Exn.protect
      ~finally:(fun () ->
        match !prepared with
        | true -> ()
        | false -> Eio.Cancel.protect runtime.close)
      ~f:(fun () ->
        match runtime.start_moderator () with
        | Ok _ ->
          prepared := true;
          Ok runtime
        | Error error -> Error error))
;;

let collect_results t handle journal persistence durable_events services runtime actor () =
  match services.Agent_session.Session_actor.job_results with
  | None -> Ok None
  | Some publisher ->
    Result_retention.collect
      ~runtime
      ~actor
      ~publisher
      ~handle
      ~journal
      ~persistence
      ~durable_events
      ~idempotency_store:t.idempotency_store
      ~limits:t.limits.job_result_collection
      ~max_frame_bytes:
        (Int.max t.limits.max_journal_payload t.limits.snapshot_payload_limit)
      ~max_events:t.limits.event_replay_capacity
;;

let create_loaded_entry
      t
      handle
      journal
      (state : Agent_session.Session_state.t)
      runtime
      writer
      persistence
      initial_events
      capacity
      actor_ref
      pending_schedule_operations
      pending_jobs
  =
  let open Result.Let_syntax in
  let%bind profile = permission_profile_revision t state.spec.permission_profile_digest in
  let%bind durable_events =
    Agent_session.Durable_event_log.create
      ~capacity:t.limits.event_replay_capacity
      initial_events
  in
  let runtime_owner = ref None in
  let%bind services =
    actor_services
      t
      handle
      journal
      persistence
      durable_events
      capacity
      runtime_owner
      ~creating_principal:state.identity.creating_principal
  in
  let actor =
    Agent_session.Session_actor.create_with_owner_lease_duration
      ~schedule_permission_timeouts:false
      ~sw:t.sw
      ~clock:(Eio.Stdenv.clock t.env)
      ~mailbox_capacity:t.limits.mailbox_capacity
      ~owner_lease_duration_ms:t.limits.owner_lease_duration_ms
      ~max_attachments:t.limits.max_attachments_per_session
      ~subscriber_capacity:t.limits.subscriber_queue_capacity
      ~compaction_env:(Some t.env)
      ~initial_state:state
      ~persistence:(Agent_session.Session_persistence.actor_persistence persistence)
      ~operation_worker:(Some runtime.Agent_session.Runtime_builder.worker)
      ~services
  in
  actor_ref := Some actor;
  match
    let open Result.Let_syntax in
    let%bind () = flush_pending_schedules actor !pending_schedule_operations in
    let%bind () = flush_pending_jobs actor !pending_jobs in
    let%bind () =
      match runtime.automatic_turn_policy with
      | None -> Ok ()
      | Some policy ->
        Agent_session.Session_actor.enable_automatic_turn_budget actor policy
    in
    let%bind moderator_snapshot = runtime.start_moderator () in
    install_moderator_if_changed actor moderator_snapshot
  with
  | Error _ as failure ->
    actor_ref := None;
    close_unregistered_runtime t handle runtime writer actor capacity;
    failure
  | Ok () ->
    let runtime =
      Runtime_owner.create ~actor ~initial:(Some runtime) ~build:(fun () ->
        build_runtime_for_actor t handle actor)
    in
    runtime_owner := Some runtime;
    (match history_source t actor state.identity.session_id with
     | Ok history_ids ->
       let entry =
         Session_registry.
           { actor
           ; history_ids
           ; runtime
           ; durable_events
           ; capacity
           ; store_handle = Some handle
           ; expire_permissions = expire_permissions t actor profile
           ; collect_results =
               collect_results
                 t
                 handle
                 journal
                 persistence
                 durable_events
                 services
                 runtime
                 actor
           ; close =
               (fun () ->
                 close_entry t handle journal persistence runtime writer actor capacity)
           }
       in
       (match state.lifecycle.observed with
        | Agent_protocol.Session.Stopped ->
          (match Runtime_owner.unload runtime with
           | Ok () -> Ok entry
           | Error _ as failure ->
             actor_ref := None;
             runtime_owner := None;
             close_unregistered_entry t handle runtime writer actor capacity;
             failure)
        | Queued_for_slot
        | Starting
        | Recovering
        | Idle
        | Running_turn _
        | Compacting _
        | Waiting_for_permission _
        | Stopping
        | Failed _ -> Ok entry)
     | Error _ as failure ->
       actor_ref := None;
       close_unregistered_entry t handle runtime writer actor capacity;
       failure)
;;

let create_unloaded_entry
      t
      handle
      journal
      (state : Agent_session.Session_state.t)
      writer
      persistence
      initial_events
      capacity
  =
  let open Result.Let_syntax in
  let%bind profile = permission_profile_revision t state.spec.permission_profile_digest in
  let%bind durable_events =
    Agent_session.Durable_event_log.create
      ~capacity:t.limits.event_replay_capacity
      initial_events
  in
  let runtime_owner = ref None in
  let%bind services =
    actor_services
      t
      handle
      journal
      persistence
      durable_events
      capacity
      runtime_owner
      ~creating_principal:state.identity.creating_principal
  in
  let actor =
    Agent_session.Session_actor.create_with_owner_lease_duration
      ~schedule_permission_timeouts:false
      ~sw:t.sw
      ~clock:(Eio.Stdenv.clock t.env)
      ~mailbox_capacity:t.limits.mailbox_capacity
      ~owner_lease_duration_ms:t.limits.owner_lease_duration_ms
      ~max_attachments:t.limits.max_attachments_per_session
      ~subscriber_capacity:t.limits.subscriber_queue_capacity
      ~compaction_env:(Some t.env)
      ~initial_state:state
      ~persistence:(Agent_session.Session_persistence.actor_persistence persistence)
      ~operation_worker:None
      ~services
  in
  let runtime =
    Runtime_owner.create ~actor ~initial:None ~build:(fun () ->
      build_runtime_for_actor t handle actor)
  in
  runtime_owner := Some runtime;
  match history_source t actor state.identity.session_id with
  | Error _ as failure ->
    runtime_owner := None;
    close_unregistered_entry t handle runtime writer actor capacity;
    failure
  | Ok history_ids ->
    Ok
      Session_registry.
        { actor
        ; history_ids
        ; runtime
        ; durable_events
        ; capacity
        ; store_handle = Some handle
        ; expire_permissions = expire_permissions t actor profile
        ; collect_results =
            collect_results
              t
              handle
              journal
              persistence
              durable_events
              services
              runtime
              actor
        ; close =
            (fun () ->
              close_entry t handle journal persistence runtime writer actor capacity)
        }
;;

let cleanup_failed_session t handle =
  ignore
    (Agent_store.Session_store.close_session t.store handle
     : (unit, Agent_store.Store_error.t) result);
  ignore
    (Agent_store.Session_index.remove
       (Agent_store.Session_store.session_index t.store)
       (Agent_store.Session_store.Handle.session_id handle)
     : (unit, Agent_store.Store_error.t) result);
  Eio.Path.rmtree
    ~missing_ok:true
    Eio.Path.(Eio.Stdenv.fs t.env / Agent_store.Session_store.Handle.directory handle)
;;

let close_runtime_writer runtime writer =
  runtime.Agent_session.Runtime_builder.close ();
  Agent_store.Commit_writer.close writer
;;

let with_observed (state : Agent_session.Session_state.t) observed =
  { state with Agent_session.Session_state.lifecycle = { state.lifecycle with observed } }
;;

let prepare_initial_capacity t state =
  let open Result.Let_syntax in
  let%bind capacity = capacity_for_state t state in
  match state.Agent_session.Session_state.lifecycle.desired, capacity with
  | Stopped, _ | Running, None -> Ok (state, capacity)
  | Running, Some capacity ->
    (match Session_capacity.try_acquire capacity ~queue_if_limited:true with
     | Acquired | Already_acquired ->
       Session_capacity.runtime_ready capacity;
       Ok (with_observed state Idle, Some capacity)
     | Queue_required _ -> Ok (with_observed state Queued_for_slot, Some capacity)
     | Rejected error -> Error error)
;;

let commit_creation t persistence state ~command_audit =
  let open Result.Let_syntax in
  let%bind transition =
    Agent_session.Session_transition.apply
      ~now:(now t)
      state
      ~delta:(Agent_session.Session_delta.Created state)
      ~payloads:
        [ Agent_protocol.Event.Durable.Payload.Session_created
            (Agent_session.Session_state.summary state)
        ]
  in
  let%map () =
    Agent_session.Session_persistence.commit
      persistence
      ~command_audit
      ~previous:state
      transition
  in
  transition
;;

let finish_creation_with_runtime
      t
      handle
      provisional
      runtime
      shell
      actor_ref
      pending_schedule_operations
      pending_jobs
      ~command_audit
  =
  let initial = install_runtime_state provisional runtime shell in
  match prepare_initial_capacity t initial with
  | Error _ as failure ->
    runtime.close ();
    failure
  | Ok (state, capacity) ->
    let session_id = state.Agent_session.Session_state.identity.session_id in
    (match create_journal t handle session_id with
     | Error error ->
       Option.iter capacity ~f:Session_capacity.release;
       runtime.close ();
       Error (protocol_of_store error)
     | Ok (journal, writer) ->
       let persistence =
         Agent_session.Session_persistence.create
           ~archive:
             (Agent_session.Compaction_archive.write
                ~env:t.env
                ~handle
                ~max_payload_length:t.limits.snapshot_payload_limit)
           ~command_accepted:(mark_command_accepted t)
           ~writer
           ~durability:t.durability
           ~previous_transaction_hash:None
       in
       let creation_result = commit_creation t persistence state ~command_audit in
       (match creation_result with
        | Error _ as failure ->
          Option.iter capacity ~f:Session_capacity.release;
          close_runtime_writer runtime writer;
          failure
        | Ok creation ->
          let state = creation.Agent_session.Session_transition.state in
          let snapshot_result =
            Agent_session.Session_persistence.install_snapshot
              ~env:t.env
              ~handle
              ~max_payload_length:t.limits.snapshot_payload_limit
              ~transaction_hash:
                (Agent_session.Session_persistence.transaction_hash persistence)
              state
            |> Result.map_error ~f:protocol_of_store
          in
          (match snapshot_result with
           | Error _ as failure ->
             Option.iter capacity ~f:Session_capacity.release;
             close_runtime_writer runtime writer;
             failure
           | Ok _ ->
             (match
                persist_metadata t handle state |> Result.map_error ~f:protocol_of_store
              with
              | Error _ as failure ->
                Option.iter capacity ~f:Session_capacity.release;
                close_runtime_writer runtime writer;
                failure
              | Ok () ->
                create_loaded_entry
                  t
                  handle
                  journal
                  state
                  runtime
                  writer
                  persistence
                  creation.events
                  capacity
                  actor_ref
                  pending_schedule_operations
                  pending_jobs))))
;;

let persist_initial_snapshot t handle journal persistence state =
  let open Result.Let_syntax in
  let%bind installed =
    Agent_session.Session_persistence.install_snapshot
      ~env:t.env
      ~handle
      ~max_payload_length:t.limits.snapshot_payload_limit
      ~transaction_hash:(Agent_session.Session_persistence.transaction_hash persistence)
      state
    |> Result.map_error ~f:protocol_of_store
  in
  ignore
    (prune_snapshot t handle journal installed : (unit, Agent_store.Store_error.t) result);
  persist_metadata t handle state |> Result.map_error ~f:protocol_of_store
;;

let finish_unloaded_creation t handle state ~command_audit =
  let open Result.Let_syntax in
  let%bind state, capacity = prepare_initial_capacity t state in
  let%bind journal, writer =
    create_journal t handle state.identity.session_id
    |> Result.map_error ~f:protocol_of_store
  in
  let persistence =
    Agent_session.Session_persistence.create
      ~archive:
        (Agent_session.Compaction_archive.write
           ~env:t.env
           ~handle
           ~max_payload_length:t.limits.snapshot_payload_limit)
      ~command_accepted:(mark_command_accepted t)
      ~writer
      ~durability:t.durability
      ~previous_transaction_hash:None
  in
  match commit_creation t persistence state ~command_audit with
  | Error _ as failure ->
    Option.iter capacity ~f:Session_capacity.release;
    Agent_store.Commit_writer.close writer;
    failure
  | Ok creation ->
    let state = creation.Agent_session.Session_transition.state in
    (match persist_initial_snapshot t handle journal persistence state with
     | Error _ as failure ->
       Option.iter capacity ~f:Session_capacity.release;
       Agent_store.Commit_writer.close writer;
       failure
     | Ok () ->
       create_unloaded_entry
         t
         handle
         journal
         state
         writer
         persistence
         creation.events
         capacity)
;;

let finish_creation t handle revision profile provisional ~command_audit =
  let actor_ref = ref None in
  let pending_schedule_operations = ref [] in
  let pending_jobs = ref [] in
  let runtime_result =
    prepare_runtime
      t
      handle
      (Authored revision)
      profile
      provisional
      ~next_history_sequence:0
      ~existing_history:None
      ~existing_moderator_snapshot:None
      ~actor_ref
      ~pending_schedule_operations
      ~pending_jobs
  in
  match runtime_result with
  | Error _ as failure -> failure
  | Ok (runtime, shell) ->
    finish_creation_with_runtime
      t
      handle
      provisional
      runtime
      shell
      actor_ref
      pending_schedule_operations
      pending_jobs
      ~command_audit
;;

let create_session
      t
      ~command_audit
      ~principal
      (request : Agent_protocol.Session.Create_request.t)
  =
  let open Result.Let_syntax in
  let%bind definition, revision = resolve_prompt t request.spec.prompt in
  let%bind workspace_definition =
    resolve_workspace_definition t definition request.spec.workspace
  in
  let%bind profile = permission_profile t definition.permission_profile in
  let session_id = Agent_protocol.Id.Session.create () in
  let final_directory =
    Agent_store.Data_root.session_path
      (Agent_store.Session_store.data_root t.store)
      session_id
  in
  let now = now t in
  let provisional = ref None in
  let handle_result =
    Agent_store.Session_store.create_session_initialized
      t.store
      ~sw:t.sw
      ~transaction_id:(Agent_protocol.Id.Transaction.create ())
      ~actor_lock_nonce:
        (Agent_protocol.Id.Transaction.create ()
         |> Agent_protocol.Id.Transaction.to_string)
      ~initialize:
        (initialize_layout
           t
           ~principal
           ~request
           ~session_id
           ~now
           ~definition
           ~revision
           ~workspace_definition
           ~profile
           ~final_directory
           ~on_state:(fun state -> provisional := Some state))
  in
  let%bind handle = Result.map_error handle_result ~f:protocol_of_store in
  let%bind provisional =
    Option.value_map
      !provisional
      ~default:
        (Error
           (unavailable
              Internal_error
              "session initializer did not return workspace state"))
      ~f:Result.return
  in
  match finish_creation t handle revision profile provisional ~command_audit with
  | Ok entry -> Ok entry
  | Error _ as failure ->
    cleanup_failed_session t handle;
    failure
;;

let validate_import_request request legacy source_path =
  let spec = request.Agent_protocol.Session.Create_request.spec in
  if not (Filename.is_absolute source_path)
  then Error (unavailable Invalid_request "legacy source path must be absolute")
  else if request.subscribe || Option.is_some request.requested_mode
  then Error (unavailable Invalid_request "legacy import cannot create an attachment")
  else (
    match
      spec.execution_host, spec.persistence, spec.liveness, spec.start_immediately
    with
    | Daemon, Durable, Detached, false ->
      Session.V5.validate (Session.to_v5 legacy)
      |> Result.map_error ~f:(fun message -> unavailable Migration_required message)
    | _ ->
      Error
        (unavailable
           Invalid_request
           "legacy import requires a stopped detached durable daemon session"))
;;

let save_legacy_provenance t handle ~source_id ~source_path legacy =
  let provenance =
    Legacy_provenance.
      { version = 1
      ; source_id
      ; source_path
      ; prompt_file = legacy.Session.prompt_file
      ; local_prompt_copy = legacy.local_prompt_copy
      ; vfs_root = legacy.vfs_root
      ; imported_at = now t
      ; diagnostics = []
      }
  in
  Agent_store.Durable_file.replace
    ~env:t.env
    ~durability:Flush_file_and_directory
    ~path:
      (Filename.concat
         (Agent_store.Session_store.Handle.archive_directory handle)
         "legacy-import.sexp")
    (Sexp.to_string_mach ([%sexp_of: Legacy_provenance.t] provenance))
  |> Result.map_error ~f:protocol_of_store
;;

let import_legacy t ~principal ~source_id ~source_path ~legacy request =
  let open Result.Let_syntax in
  let%bind () = validate_import_request request legacy source_path in
  let%bind definition, revision = resolve_prompt t request.spec.prompt in
  let%bind workspace_definition =
    resolve_workspace_definition t definition request.spec.workspace
  in
  let%bind profile = permission_profile t definition.permission_profile in
  let session_id = Agent_protocol.Id.Session.create () in
  let final_directory =
    Agent_store.Data_root.session_path
      (Agent_store.Session_store.data_root t.store)
      session_id
  in
  let provisional = ref None in
  let handle_result =
    Agent_store.Session_store.create_session_initialized
      t.store
      ~sw:t.sw
      ~transaction_id:(Agent_protocol.Id.Transaction.create ())
      ~actor_lock_nonce:
        (Agent_protocol.Id.Transaction.create ()
         |> Agent_protocol.Id.Transaction.to_string)
      ~initialize:
        (initialize_layout
           t
           ~principal
           ~request
           ~session_id
           ~now:(now t)
           ~definition
           ~revision
           ~workspace_definition
           ~profile
           ~final_directory
           ~on_state:(fun state -> provisional := Some state))
  in
  let%bind handle = Result.map_error handle_result ~f:protocol_of_store in
  let result =
    let open Result.Let_syntax in
    let%bind provisional =
      Option.value_map
        !provisional
        ~default:(Error (unavailable Internal_error "legacy import state is unavailable"))
        ~f:Result.return
    in
    let%bind state = imported_state provisional legacy in
    let%bind () = save_legacy_provenance t handle ~source_id ~source_path legacy in
    finish_unloaded_creation t handle state ~command_audit:None
  in
  match result with
  | Ok _ as success -> success
  | Error _ as failure ->
    cleanup_failed_session t handle;
    failure
;;

let close_recovery_handle t handle =
  ignore
    (Agent_store.Session_store.close_session t.store handle
     : (unit, Agent_store.Store_error.t) result)
;;

let corrupt message = protocol_of_store (Agent_store.Store_error.Corrupt message)

let validate_snapshot handle installed state =
  let snapshot = installed.Agent_store.Snapshot.snapshot in
  let session_id = Agent_store.Session_store.Handle.session_id handle in
  let prompt_revision =
    Agent_protocol.Id.Prompt_revision.to_string
      state.Agent_session.Session_state.spec.prompt_revision_id
  in
  if Agent_protocol.Id.Session.compare session_id state.identity.session_id <> 0
  then Error (corrupt "snapshot session identity does not match its directory")
  else if not (String.equal snapshot.prompt_artifact prompt_revision)
  then Error (corrupt "snapshot prompt artifact does not match session state")
  else if
    not
      (String.equal
         snapshot.workspace_identity
         state.spec.workspace_instance.conflict_domain)
  then Error (corrupt "snapshot workspace identity does not match session state")
  else if
    not (Int64.equal snapshot.transaction_sequence state.counters.transaction_sequence)
  then Error (corrupt "snapshot transaction counter does not match session state")
  else Ok ()
;;

let initial_recovery_state t handle =
  let open Result.Let_syntax in
  let%bind installed =
    Agent_store.Snapshot.load_current
      ~env:t.env
      ~directory:(Agent_store.Session_store.Handle.snapshot_directory handle)
      ~max_payload_length:t.limits.snapshot_payload_limit
    |> Result.map_error ~f:protocol_of_store
  in
  let%bind installed =
    Option.value_map
      installed
      ~default:(Error (corrupt "durable session has no valid snapshot"))
      ~f:Result.return
  in
  let%bind state =
    Agent_session.Session_persistence.restore_snapshot installed.snapshot.payload
    |> Result.map_error ~f:protocol_of_store
  in
  let%map () = validate_snapshot handle installed state in
  state
;;

let open_recovery t handle initial =
  let open Result.Let_syntax in
  let%bind journal =
    Agent_store.Journal.open_existing
      ~env:t.env
      ~directory:(Agent_store.Session_store.Handle.journal_directory handle)
      ~max_payload_length:t.limits.max_journal_payload
      ~max_segment_bytes:t.limits.max_segment_bytes
      ~max_segment_frames:t.limits.max_segment_frames
    |> Result.map_error ~f:protocol_of_store
  in
  let validate state =
    Agent_session.Session_state.validate state
    |> Result.map_error ~f:(fun error ->
      Agent_store.Store_error.Corrupt error.Agent_protocol.Error.message)
  in
  let%map recovery =
    Agent_store.Recovery.load
      ~env:t.env
      ~journal
      ~snapshot_directory:(Agent_store.Session_store.Handle.snapshot_directory handle)
      ~max_snapshot_payload_length:t.limits.snapshot_payload_limit
      ~session_id:initial.Agent_session.Session_state.identity.session_id
      ~initial
      ~restore_snapshot:Agent_session.Session_persistence.restore_snapshot
      ~apply:Agent_session.Session_persistence.apply_transaction
      ~validate
    |> Result.map_error ~f:protocol_of_store
  in
  journal, recovery
;;

let recovered_revision = restore_state_source
let recovered_profile = restore_state_profile

let verify_recovered_workspace t state =
  Agent_session.Workspace_resolver.verify_available
    ~env:t.env
    state.Agent_session.Session_state.spec.workspace_instance
  |> Result.map_error ~f:protocol_of_store
;;

let recovery_reservation t first =
  let count = t.limits.moderator_reservation_size in
  let maximum = Int64.of_int Int.max_value in
  if count < 0
  then Error (unavailable Configuration_invalid "negative moderator reservation size")
  else if Int64.(first > maximum - of_int count)
  then Error (unavailable Invalid_state "history sequence space is exhausted")
  else Ok (Int64.to_int_exn first, Int64.(first + of_int count))
;;

let interrupted_operation now operation =
  { operation with
    Agent_protocol.Operation.state =
      Interrupted
        { reason = "daemon restarted while the operation was active"; retryable = true }
  ; updated_at = now
  }
;;

let interrupted_permission now (permission : Agent_protocol.Permission.t) =
  { permission with
    state = Cancelled
  ; resolution =
      Some
        { choice = Deny
        ; principal_id = None
        ; resolved_at = now
        ; reason = Some "daemon restarted while approval was pending"
        }
  }
;;

let interrupted_permissions state now =
  List.filter_map state.Agent_session.Session_state.permissions ~f:(fun permission ->
    if Agent_protocol.Permission.equal_state permission.state Pending
    then Some (interrupted_permission now permission)
    else None)
;;

let is_permission_review_job (job : Agent_protocol.Job.t) =
  match job.payload with
  | `Object fields ->
    List.Assoc.find fields "type" ~equal:String.equal
    |> Option.value_map ~default:false ~f:(function
      | `String value -> String.equal value "permission_review"
      | _ -> false)
  | _ -> false
;;

let interrupted_reviewer_job now (job : Agent_protocol.Job.t) =
  if is_permission_review_job job
  then (
    match job.status with
    | Running ->
      Some
        { job with
          status = Interrupted "daemon restarted while permission reviewer was active"
        ; completed_at = Some now
        ; delivery = Pending
        }
    | Queued ->
      Some { job with status = Cancelled; completed_at = Some now; delivery = Pending }
    | Waiting_permission _
    | Waiting_completion _
    | Succeeded
    | Failed _
    | Cancelled
    | Interrupted _ -> None)
  else None
;;

let interrupted_reviewer_jobs state now =
  List.filter_map state.Agent_session.Session_state.jobs ~f:(interrupted_reviewer_job now)
;;

let recovery_attachment_deltas state ~now =
  let disconnect_owner attachment lease disconnect_grace_ms =
    let grace =
      now
      |> Agent_protocol.Timestamp.to_time_ns
      |> Fn.flip Time_ns.add (Time_ns.Span.of_ms (Float.of_int disconnect_grace_ms))
      |> Agent_protocol.Timestamp.of_time_ns
    in
    let existing_deadline =
      Option.value
        lease.Agent_protocol.Session.Owner_lease.disconnect_grace_until
        ~default:grace
    in
    let deadline =
      List.min_elt
        [ lease.expires_at; existing_deadline ]
        ~compare:Agent_protocol.Timestamp.compare
      |> Option.value_exn
    in
    let lease = { lease with disconnect_grace_until = Some deadline } in
    Agent_session.Session_delta.Attachment_added
      { attachment with Agent_protocol.Session.Attachment.owner_lease = Some lease }
  in
  List.map state.Agent_session.Session_state.attachments ~f:(fun attachment ->
    match
      ( attachment.Agent_protocol.Session.Attachment.mode
      , attachment.owner_lease
      , state.spec.protocol.liveness )
    with
    | Owner_read_write, Some lease, Owner_bound { disconnect_grace_ms; _ } ->
      disconnect_owner attachment lease disconnect_grace_ms
    | _ -> Agent_session.Session_delta.Attachment_removed attachment.id)
;;

let recovery_transition
      ~parent_stop
      state
      ~now
      ~reserved_history_through
      ~observed
      ~(invocations : Agent_session.Invocation_recovery.t)
  =
  let lifecycle =
    Agent_session.Session_state.Lifecycle.
      { desired =
          (if Option.exists parent_stop ~f:(fun parent -> parent.stop)
           then Stopped
           else state.Agent_session.Session_state.lifecycle.desired)
      ; observed
      }
  in
  let attachment_deltas = recovery_attachment_deltas state ~now in
  let owner_was_attached =
    List.exists state.attachments ~f:(fun attachment ->
      Agent_protocol.Session.equal_attachment_mode attachment.mode Owner_read_write)
  in
  let operation_delta, operation_payload =
    match state.active_operation with
    | None -> [], []
    | Some operation ->
      ( [ Agent_session.Session_delta.Active_operation_changed None ]
      , [ Agent_protocol.Event.Durable.Payload.Operation_interrupted
            (interrupted_operation now operation)
        ] )
  in
  let permissions = interrupted_permissions state now in
  let permission_deltas =
    List.map permissions ~f:(fun permission ->
      Agent_session.Session_delta.Permission_changed permission)
  in
  let permission_payloads =
    List.map permissions ~f:(fun permission ->
      Agent_protocol.Event.Durable.Payload.Permission_resolved permission)
  in
  let reviewer_jobs = interrupted_reviewer_jobs state now in
  let reviewer_job_deltas =
    List.map reviewer_jobs ~f:(fun job -> Agent_session.Session_delta.Job_changed job)
  in
  let reviewer_job_payloads =
    List.map reviewer_jobs ~f:(fun job ->
      Agent_protocol.Event.Durable.Payload.Job_state_changed job)
  in
  Agent_session.Session_transition.apply
    ~now
    state
    ~delta:
      (Batch
         ([ Agent_session.Session_delta.History_block_reserved reserved_history_through
          ; Lifecycle_changed lifecycle
          ]
          @ operation_delta
          @ invocations.deltas
          @ permission_deltas
          @ reviewer_job_deltas
          @ attachment_deltas
          @
          match parent_stop with
          | None -> []
          | Some { stop = true; _ } -> [ Initial_start_consumed ]
          | Some { stop = false; epoch; _ } -> [ Parent_stop_epoch_changed epoch ]))
    ~payloads:
      (operation_payload
       @ (if List.is_empty invocations.appended
          then []
          else
            [ Agent_protocol.Event.Durable.Payload.History_appended invocations.appended ])
       @ permission_payloads
       @ reviewer_job_payloads
       @ Option.to_list
           (Option.some_if
              owner_was_attached
              (Agent_protocol.Event.Durable.Payload.Attachment_owner_changed None))
       @ [ Agent_protocol.Event.Durable.Payload.Session_state_changed
             { desired_state = lifecycle.desired; observed_state = lifecycle.observed }
         ])
;;

let create_recovery_writer t journal recovery session_id =
  if Int64.equal recovery.Agent_store.Recovery.latest_transaction_sequence Int64.max_value
  then Error (unavailable Invalid_state "transaction sequence space is exhausted")
  else
    Agent_store.Commit_writer.create
      ~sw:t.sw
      ~journal
      ~session_id
      ~next_transaction_sequence:Int64.(recovery.latest_transaction_sequence + 1L)
      ~previous_transaction_hash:recovery.latest_transaction_hash
      ~queue_capacity:t.limits.commit_queue_capacity
    |> Result.map_error ~f:protocol_of_store
;;

let commit_recovery_boundary
      ~parent_stop
      t
      persistence
      state
      reserved_history_through
      observed
      invocations
  =
  let open Result.Let_syntax in
  let%bind transition =
    recovery_transition
      ~parent_stop
      state
      ~now:(now t)
      ~reserved_history_through
      ~observed
      ~invocations
  in
  let%map () =
    Agent_session.Session_persistence.commit
      persistence
      ~command_audit:None
      ~previous:state
      transition
  in
  transition
;;

let recovered_events recovery =
  Result.all
    (List.map recovery.Agent_store.Recovery.transactions ~f:(fun transaction ->
       Agent_session.Session_persistence.durable_events transaction))
  |> Result.map ~f:List.concat
  |> Result.map_error ~f:protocol_of_store
;;

let reconcile_command_audits t recovery =
  let open Result.Let_syntax in
  let%bind accepted =
    List.filter_map recovery.Agent_store.Recovery.transactions ~f:(fun transaction ->
      Option.map transaction.Agent_store.Transaction.command_audit ~f:(fun encoded ->
        encoded, transaction.transaction_sequence))
    |> List.map ~f:(fun (encoded, sequence) ->
      Result.map
        (Agent_store.Idempotency_store.Command_audit.decode encoded)
        ~f:(fun audit -> audit, sequence))
    |> Result.all
    |> Result.map_error ~f:protocol_of_store
  in
  Agent_store.Idempotency_store.reconcile_accepted t.idempotency_store accepted
  |> Result.map ~f:(fun _ -> ())
  |> Result.map_error ~f:protocol_of_store
;;

let prepare_recovery_capacity t state =
  let open Result.Let_syntax in
  let%bind capacity = capacity_for_state t state in
  match state.Agent_session.Session_state.lifecycle.desired, capacity with
  | Stopped, _ -> Ok (Agent_protocol.Session.Stopped, capacity)
  | Running, None -> Ok (Idle, None)
  | Running, Some capacity ->
    (match Session_capacity.try_acquire capacity ~queue_if_limited:true with
     | Acquired | Already_acquired ->
       Session_capacity.runtime_ready capacity;
       Ok (Idle, Some capacity)
     | Queue_required _ | Rejected _ -> Ok (Queued_for_slot, Some capacity))
;;

let commit_moderator_snapshot t persistence state moderator =
  let open Result.Let_syntax in
  let%bind transition =
    Agent_session.Session_transition.apply
      ~now:(now t)
      state
      ~delta:(Agent_session.Session_delta.Moderator_changed moderator)
      ~payloads:[]
  in
  let%map () =
    Agent_session.Session_persistence.commit
      persistence
      ~command_audit:None
      ~previous:state
      transition
  in
  transition.state
;;

let persist_recovered_state t handle journal persistence state =
  let open Result.Let_syntax in
  let%bind installed =
    Agent_session.Session_persistence.install_snapshot
      ~env:t.env
      ~handle
      ~max_payload_length:t.limits.snapshot_payload_limit
      ~transaction_hash:(Agent_session.Session_persistence.transaction_hash persistence)
      state
    |> Result.map_error ~f:protocol_of_store
  in
  ignore
    (prune_snapshot t handle journal installed : (unit, Agent_store.Store_error.t) result);
  persist_metadata t handle state |> Result.map_error ~f:protocol_of_store
;;

let recover_runtime
      t
      handle
      revision
      profile
      persistence
      state
      first_sequence
      actor_ref
      pending_schedule_operations
      pending_jobs
  =
  let open Result.Let_syntax in
  let%bind history =
    Agent_session.History_codec.all_of_protocol
      state.Agent_session.Session_state.conversation.canonical_history
  in
  let runtime_result =
    prepare_runtime
      t
      handle
      revision
      profile
      state
      ~next_history_sequence:first_sequence
      ~existing_history:(Some history)
      ~existing_moderator_snapshot:state.moderator
      ~actor_ref
      ~pending_schedule_operations
      ~pending_jobs
  in
  match runtime_result with
  | Error _ as failure -> failure
  | Ok (runtime, shell) -> Ok ({ state with shell }, runtime)
;;

let recover_open_handle t handle =
  let open Result.Let_syntax in
  let%bind initial = initial_recovery_state t handle in
  let%bind journal, recovery = open_recovery t handle initial in
  let%bind () = reconcile_command_audits t recovery in
  let state = recovery.Agent_store.Recovery.state in
  let%bind parent_stop = parent_stop_recovery t state in
  let stopping = Option.exists parent_stop ~f:(fun parent -> parent.stop) in
  let%bind revision = recovered_revision t state in
  let%bind () =
    match state.lifecycle.desired with
    | Stopped -> Ok ()
    | Running when not stopping -> check_source_for_execution t state revision
    | Running -> Ok ()
  in
  let%bind profile = recovered_profile t state in
  let%bind () = verify_recovered_workspace t state in
  let%bind recovery_first = preparation_sequence t state in
  let%bind invocations =
    Agent_session.Invocation_recovery.plan
      ~state
      ~namespace:(Agent_protocol.Id.Session.to_string state.identity.session_id)
      ~first_sequence:recovery_first
      ~reason:"daemon restarted before the invocation recorded an outcome"
  in
  let%bind first_sequence, reserved_history_through =
    recovery_reservation t (Int64.of_int invocations.next_sequence)
  in
  let%bind durable_events = recovered_events recovery in
  let%bind observed, capacity =
    prepare_recovery_capacity
      t
      (if stopping
       then { state with lifecycle = { desired = Stopped; observed = Stopped } }
       else state)
  in
  let%bind writer = create_recovery_writer t journal recovery state.identity.session_id in
  let persistence =
    Agent_session.Session_persistence.create
      ~archive:
        (Agent_session.Compaction_archive.write
           ~env:t.env
           ~handle
           ~max_payload_length:t.limits.snapshot_payload_limit)
      ~command_accepted:(mark_command_accepted t)
      ~writer
      ~durability:t.durability
      ~previous_transaction_hash:recovery.latest_transaction_hash
  in
  match
    commit_recovery_boundary
      ~parent_stop
      t
      persistence
      state
      reserved_history_through
      observed
      invocations
  with
  | Error _ as failure ->
    Option.iter capacity ~f:Session_capacity.release;
    Agent_store.Commit_writer.close writer;
    failure
  | Ok recovery_transition ->
    let state = recovery_transition.Agent_session.Session_transition.state in
    let durable_events = durable_events @ recovery_transition.events in
    (match state.lifecycle.observed with
     | Agent_protocol.Session.Stopped ->
       (match persist_recovered_state t handle journal persistence state with
        | Error _ as failure ->
          Option.iter capacity ~f:Session_capacity.release;
          Agent_store.Commit_writer.close writer;
          failure
        | Ok () ->
          let%bind entry =
            create_unloaded_entry
              t
              handle
              journal
              state
              writer
              persistence
              durable_events
              capacity
          in
          (match parent_stop with
           | Some { stop = true; reference; epoch } ->
             (match
                Agent_session.Session_actor.stop_delegated_at_epoch
                  ~force:true
                  entry.actor
                  ~reference
                  ~epoch
              with
              | Ok _ ->
                (match Runtime_owner.unload_and_wait entry.runtime with
                 | Ok () -> Ok entry
                 | Error failure ->
                   entry.close ();
                   Error failure)
              | Error failure ->
                entry.close ();
                Error failure)
           | None | Some { stop = false; _ } -> Ok entry))
     | Queued_for_slot
     | Starting
     | Recovering
     | Idle
     | Running_turn _
     | Compacting _
     | Waiting_for_permission _
     | Stopping
     | Failed _ ->
       let actor_ref = ref None in
       let pending_schedule_operations = ref [] in
       let pending_jobs = ref [] in
       (match
          recover_runtime
            t
            handle
            revision
            profile
            persistence
            state
            first_sequence
            actor_ref
            pending_schedule_operations
            pending_jobs
        with
        | Error _ as failure ->
          Option.iter capacity ~f:Session_capacity.release;
          Agent_store.Commit_writer.close writer;
          failure
        | Ok (state, runtime) ->
          (match persist_recovered_state t handle journal persistence state with
           | Error _ as failure ->
             Option.iter capacity ~f:Session_capacity.release;
             close_runtime_writer runtime writer;
             failure
           | Ok () ->
             create_loaded_entry
               t
               handle
               journal
               state
               runtime
               writer
               persistence
               durable_events
               capacity
               actor_ref
               pending_schedule_operations
               pending_jobs)))
;;

let recover_index_entry t index_entry =
  let session_id = index_entry.Agent_store.Session_index.Entry.session.id in
  let handle_result =
    Agent_store.Session_store.open_session
      t.store
      ~sw:t.sw
      ~actor_lock_nonce:
        (Agent_protocol.Id.Transaction.create ()
         |> Agent_protocol.Id.Transaction.to_string)
      session_id
    |> Result.map_error ~f:protocol_of_store
  in
  match handle_result with
  | Error _ as failure -> failure
  | Ok handle ->
    (match recover_open_handle t handle with
     | Ok entry -> Ok entry
     | Error _ as failure ->
       close_recovery_handle t handle;
       failure)
;;

let recover_session = recover_index_entry

let checkpoint_recovered_index t entry =
  match entry.Session_registry.store_handle with
  | None -> Error (corrupt "recovered durable session has no store handle")
  | Some handle ->
    Agent_session.Session_actor.checkpoint entry.actor ~persist:(fun state ->
      persist_metadata t handle state |> Result.map_error ~f:protocol_of_store)
;;

let complete_index_recovery t entries =
  if not (Agent_store.Session_store.index_was_rebuilt t.store)
  then Ok ()
  else
    let open Result.Let_syntax in
    let%bind () =
      List.fold_result entries ~init:() ~f:(fun () entry ->
        checkpoint_recovered_index t entry)
    in
    Agent_store.Session_store.complete_index_recovery t.store
    |> Result.map_error ~f:protocol_of_store
;;

let index_requires_load entry =
  let session = entry.Agent_store.Session_index.Entry.session in
  Agent_protocol.Session.equal_desired_state session.desired_state Running
  || entry.runnable_job_count > 0
  || entry.deliverable_job_count > 0
  || Option.is_some entry.earliest_schedule_due
  || Option.is_some entry.owner_grace_deadline
  || entry.pending_initial_start
;;

let recover_sessions t =
  let open Result.Let_syntax in
  let all =
    Agent_store.Session_store.list_sessions t.store
    |> List.filter ~f:(fun entry -> not entry.Agent_store.Session_index.Entry.archived)
  in
  let indexed =
    all
    |> List.filter ~f:(fun entry ->
      Agent_store.Session_store.index_was_rebuilt t.store || index_requires_load entry)
  in
  let%bind records =
    Agent_store.Delegation_store.with_records
      (Agent_store.Session_store.delegations t.store)
      ~max_records:t.limits.delegation_recovery_max_count
      ~max_bytes:t.limits.delegation_recovery_max_bytes
      ~f:(fun records -> Ok records)
    |> Result.map_error ~f:protocol_of_store
  in
  let entries =
    String.Map.of_alist_exn
      (List.map all ~f:(fun entry ->
         ( Agent_protocol.Id.Session.to_string
             entry.Agent_store.Session_index.Entry.session.id
         , entry )))
  in
  let parents =
    String.Map.of_alist_exn
      (List.map records ~f:(fun record ->
         ( Agent_protocol.Id.Session.to_string
             record.Agent_store.Delegation_store.admission.child_session_id
         , record.key.parent_session_id )))
  in
  let visiting = Hash_set.create (module String) in
  let heights = Hashtbl.create (module String) in
  let ordered = ref [] in
  let rec visit depth id =
    let key = Agent_protocol.Id.Session.to_string id in
    if depth > t.limits.delegation_max_depth || Hash_set.mem visiting key
    then
      Error (corrupt "generated recovery ancestry is cyclic or exceeds its depth limit")
    else (
      match Hashtbl.find heights key with
      | Some height ->
        if height > t.limits.delegation_max_depth - depth
        then Error (corrupt "generated recovery ancestry exceeds its depth limit")
        else Ok height
      | None ->
        (match Map.find entries key with
         | None -> Error (corrupt "generated recovery requires a missing parent session")
         | Some entry ->
           Hash_set.add visiting key;
           let%bind height =
             match entry.session.spec.prompt, entry.session.desired_state with
             | Generated _, (Running | Stopped) ->
               (match Map.find parents key with
                | None ->
                  Error
                    (corrupt "generated recovery requires a private delegation record")
                | Some parent ->
                  (match Map.mem entries (Agent_protocol.Id.Session.to_string parent) with
                   | true -> Result.map (visit (depth + 1) parent) ~f:(( + ) 1)
                   | false -> Ok 0))
             | _ -> Ok 0
           in
           Hash_set.remove visiting key;
           Hashtbl.set heights ~key ~data:height;
           ordered := entry :: !ordered;
           Ok height))
  in
  let%bind () =
    List.fold_result indexed ~init:() ~f:(fun () entry ->
      Result.map (visit 0 entry.session.id) ~f:ignore)
  in
  let rollback recovered =
    List.iter recovered ~f:(fun (id, entry) ->
      ignore (Session_registry.remove t.registry id : Session_registry.entry option);
      entry.Session_registry.close ())
  in
  let rec loop recovered = function
    | [] -> Ok (List.rev_map recovered ~f:snd)
    | index_entry :: rest ->
      (match recover_index_entry t index_entry with
       | Ok entry ->
         let id = index_entry.session.id in
         (match Session_registry.add t.registry ~session_id:id entry with
          | Ok () -> loop ((id, entry) :: recovered) rest
          | Error _ as failure ->
            entry.close ();
            rollback recovered;
            failure)
       | Error _ as failure ->
         rollback recovered;
         failure)
  in
  loop [] (List.rev !ordered)
;;

let initialize_generated_layout t state ~staging_directory =
  let module Store = Agent_store in
  let open Result.Let_syntax in
  let%bind journal =
    Store.Journal.create
      ~env:t.env
      ~directory:(Filename.concat staging_directory "journal")
      ~max_payload_length:t.limits.max_journal_payload
      ~max_segment_bytes:t.limits.max_segment_bytes
      ~max_segment_frames:t.limits.max_segment_frames
  in
  let%bind writer =
    Store.Commit_writer.create
      ~sw:t.sw
      ~journal
      ~session_id:state.Agent_session.Session_state.identity.session_id
      ~next_transaction_sequence:1L
      ~previous_transaction_hash:None
      ~queue_capacity:t.limits.commit_queue_capacity
  in
  Exn.protect
    ~finally:(fun () -> Store.Commit_writer.close writer)
    ~f:(fun () ->
      let persistence =
        Agent_session.Session_persistence.create
          ~archive:(fun _ _ -> Error (corrupt "creation cannot archive history"))
          ~command_accepted:(fun _ _ -> ())
          ~writer
          ~durability:Flush
          ~previous_transaction_hash:None
      in
      let%bind transition =
        commit_creation t persistence state ~command_audit:None
        |> Result.map_error ~f:(fun error -> Store.Store_error.Corrupt error.message)
      in
      let state = transition.Agent_session.Session_transition.state in
      let%bind _ =
        Agent_session.Session_persistence.install_snapshot_at
          ~env:t.env
          ~directory:(Filename.concat staging_directory "snapshot")
          ~max_payload_length:t.limits.snapshot_payload_limit
          ~transaction_hash:
            (Agent_session.Session_persistence.transaction_hash persistence)
          state
      in
      let%map () = Store.Durable_file.sync_directory ~env:t.env ~path:staging_directory in
      metadata state)
;;

let with_generated_creation_lock t f =
  let outcome =
    Eio.Mutex.use_rw ~protect:true t.generated_creation_mutex (fun () ->
      try Ok (f ()) with
      | exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ()))
  in
  match outcome with
  | Ok result -> result
  | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
;;

let prepare_session_start t entry =
  with_generated_creation_lock t (fun () ->
    let open Result.Let_syntax in
    let%bind state = Agent_session.Session_actor.state entry.Session_registry.actor in
    match state.spec.delegation with
    | None -> Ok ()
    | Some reference ->
      let%bind parent, _ = generated_parent ~check_stop_epoch:false t state in
      let%bind current = Agent_session.Session_actor.state parent.actor in
      (match
         Option.equal Int64.equal state.parent_stop_epoch (Some current.stop_epoch)
       with
       | true -> Ok ()
       | false ->
         Delegation_lifecycle.stop_owned
           ~parent_stop_epoch:current.stop_epoch
           ~clock:(Eio.Stdenv.clock t.env)
           ~delegations:(Agent_store.Session_store.delegations t.store)
           ~reference
           ~actor:entry.actor
           ~runtime:entry.runtime
           ()
         |> Result.map ~f:ignore))
;;

let resume_generated_initial_start t entry =
  let module A = Agent_session.Session_actor in
  let module D = Agent_store.Delegation_store in
  let open Result.Let_syntax in
  let%bind state = A.state entry.Session_registry.actor in
  match state.pending_initial_start, state.spec.delegation with
  | false, _ -> Ok ()
  | true, None -> Error (corrupt "generated initial start has no delegation")
  | true, Some reference ->
    (* A temporarily queued or compacting ancestor must not turn a durable start
       request into a permanent failure. Terminal loss of authority is different. *)
    let rec ready depth reference =
      let%bind () =
        match depth < t.limits.delegation_max_depth with
        | true -> Ok ()
        | false ->
          Error
            (unavailable
               Permission_denied
               "initial start exceeds delegation ancestry limit")
      in
      let%bind record =
        D.resolve (Agent_store.Session_store.delegations t.store) reference
        |> Result.map_error ~f:protocol_of_store
      in
      match record.stage, record.revocation with
      | _, Some _ ->
        Error (unavailable Permission_denied "initial start delegation was revoked")
      | (Reserved | Artifact_installed | Child_installed), None -> Ok false
      | Linked, None ->
        let%bind parent =
          Session_registry.find t.registry record.key.parent_session_id
          |> Result.of_option
               ~error:
                 (unavailable Permission_denied "initial start parent is unavailable")
        in
        let%bind current = A.state parent.actor in
        let%bind () =
          match
            depth = 0
            && not
                 (Int64.equal
                    current.stop_epoch
                    (Option.value record.admission.parent_stop_epoch ~default:0L))
          with
          | false -> Ok ()
          | true ->
            Error
              (unavailable
                 Permission_denied
                 "initial start parent stopped after admission")
        in
        let%bind fingerprint = Agent_session.Delegation_authority.fingerprint current in
        (match
           ( current.lifecycle.desired
           , current.halted
           , current.failure
           , String.equal fingerprint record.admission.authority_sha256 )
         with
         | Running, false, None, true ->
           (match current.lifecycle.observed with
            | Idle | Running_turn _ | Waiting_for_permission _ ->
              (match current.spec.delegation with
               | None -> Ok true
               | Some ancestor -> ready (depth + 1) ancestor)
            | Stopped
            | Queued_for_slot
            | Starting
            | Recovering
            | Compacting _
            | Stopping
            | Failed _ -> Ok false)
         | _ ->
           Error (unavailable Permission_denied "initial start parent authority ended"))
    in
    let attempt () =
      let%bind is_ready = ready 0 reference in
      match is_ready with
      | false -> Ok ()
      | true ->
        let%bind () = Runtime_owner.ensure_loaded entry.runtime in
        let%bind still_ready = ready 0 reference in
        (match still_ready with
         | false -> Ok ()
         | true ->
           A.start_initial_delegated
             ?expected_parent_stop_epoch:state.parent_stop_epoch
             entry.actor
             ~reference
           |> Result.map ~f:ignore)
    in
    (match attempt () with
     | Ok () -> Ok ()
     | Error failure
       when failure.retryable
            ||
            match failure.code with
            | Persistence_error | Journal_corrupt | Interrupted | Server_shutting_down ->
              true
            | _ -> false -> Error failure
     | Error failure ->
       (* Authority may have become temporarily unavailable while runtime loading
          yielded. Leave that intent pending instead of recording a terminal error. *)
       (match ready 0 reference with
        | Ok false -> Ok ()
        | Ok true | Error _ ->
          A.fail_initial_delegated entry.actor ~reference failure |> Result.map ~f:ignore))
;;

let resume_generated_initial_starts t =
  with_generated_creation_lock t (fun () ->
    Agent_store.Session_store.list_sessions t.store
    |> List.filter ~f:(fun entry ->
      entry.Agent_store.Session_index.Entry.pending_initial_start && not entry.archived)
    |> List.iter ~f:(fun indexed ->
      match Session_registry.find t.registry indexed.session.id with
      | None -> ()
      | Some entry ->
        ignore
          (resume_generated_initial_start t entry : (unit, Agent_protocol.Error.t) result)))
;;

let create_generated_session
      ?(start_immediately = false)
      t
      ~parent_session_id
      ~idempotency_key
      ~display_name
      definition
  =
  let module P = Agent_protocol in
  let module A = Agent_session.Session_actor in
  let module State = Agent_session.Session_state in
  let module G = Agent_session.Generated_definition in
  let module C = Chat_response.Tool_capability in
  let module D = Agent_store.Delegation_store in
  let module S = Agent_store.Session_store in
  let open Result.Let_syntax in
  let run () =
    let%bind parent =
      match
        t.qualify_chatml_extensions, Session_registry.find t.registry parent_session_id
      with
      | true, Some parent -> Ok parent
      | _ ->
        Error (unavailable Invalid_state "generated creation requires a qualified parent")
    in
    Runtime_owner.with_background_runtime parent.runtime (fun runtime ->
      let%bind before = A.state parent.actor in
      let%bind authority_sha256 = Agent_session.Delegation_authority.fingerprint before in
      let check_parent current =
        let%bind fingerprint = Agent_session.Delegation_authority.fingerprint current in
        match current.State.lifecycle.desired, current.halted, current.failure with
        | Running, false, None
          when String.equal fingerprint authority_sha256
               && Int64.equal current.stop_epoch before.stop_epoch -> Ok ()
        | _ ->
          Error
            (unavailable Permission_denied "parent no longer permits generated creation")
      in
      let%bind () = check_parent before in
      let%bind principal_id =
        Result.of_option
          before.identity.creating_principal
          ~error:
            (unavailable Permission_denied "generated creation needs a durable principal")
      in
      let%bind native =
        Result.of_option
          runtime.Agent_session.Runtime_builder.native_runtime
          ~error:(unavailable Invalid_state "parent native resources are unavailable")
      in
      let%bind capabilities =
        Lazy.force native.capabilities
        |> Result.map_error ~f:(fun error ->
          unavailable Permission_denied error.C.message)
      in
      let%bind () =
        C.references
          (Chat_response.Generated_admission.capabilities (G.admission definition))
        |> List.fold_result ~init:() ~f:(fun () reference ->
          C.resolve capabilities ~id:reference.id ~fingerprint:reference.fingerprint
          |> Result.map ~f:ignore
          |> Result.map_error ~f:(fun error ->
            unavailable Permission_denied error.C.message))
      in
      let artifact = G.artifact definition in
      let%bind protocol =
        P.Session.Spec.create
          ~execution_host:Daemon
          ~prompt:(Generated artifact.revision_id)
          ~workspace:before.spec.protocol.workspace
          ~liveness:Detached
          ~persistence:Durable
          ~permission_profile:before.spec.permission_profile
          ~start_immediately
          ?display_name
          ~labels:[]
          ()
      in
      let key : D.Key.t =
        { parent_session_id
        ; parent_generation = before.identity.generation
        ; principal_id
        ; idempotency_key
        }
      in
      let request_sha256 =
        [%sexp
          (if start_immediately
           then "ochat.generated-create.running.v1"
           else "ochat.generated-create.stopped.v1"
           : string)
        , (Chat_response.Generated_admission.source_fingerprint (G.admission definition)
           : string)
        , (G.capability_pins definition : (string * string) list)
        , (display_name : string option)]
        |> Sexp.to_string_mach
        |> Chatmd_shell_spec.Source_ref.digest
      in
      let ledger = S.delegations t.store in
      let candidate : D.Admission.t =
        { child_session_id = P.Id.Session.create ()
        ; revision_id = artifact.revision_id
        ; transaction_id = P.Id.Transaction.create ()
        ; manifest_sha256 = artifact.manifest_sha256
        ; parent_revision_id = before.spec.prompt_revision_id
        ; parent_stop_epoch = Some before.stop_epoch
        ; authority_sha256
        ; capability_pins = G.capability_pins definition
        ; lifetime = Owned
        ; created_at = artifact.created_at
        }
      in
      Eio.Cancel.protect (fun () ->
        let%bind reservation =
          D.reserve
            ledger
            ~key
            ~request_sha256
            ~admission:candidate
            ~max_records:t.limits.delegation_recovery_max_count
            ~max_bytes:t.limits.delegation_recovery_max_bytes
          |> Result.map_error ~f:protocol_of_store
        in
        let%bind record =
          match reservation with
          | (New record | Replay record) when Option.is_none record.revocation ->
            Ok record
          | New _ | Replay _ ->
            Error (unavailable Permission_denied "generated admission was revoked")
          | Conflict _ ->
            Error (unavailable Conflict "generated creation key has different inputs")
        in
        let%bind () =
          match record.stage with
          | Linked -> Ok ()
          | Reserved | Artifact_installed | Child_installed ->
            (match
               Int64.equal
                 before.stop_epoch
                 (Option.value record.admission.parent_stop_epoch ~default:0L)
             with
             | true -> Ok ()
             | false ->
               let%bind _ =
                 D.revoke ledger record Parent_stopped
                 |> Result.map_error ~f:protocol_of_store
               in
               Error
                 (unavailable
                    Permission_denied
                    "parent stopped since generated creation admission"))
        in
        let%bind () =
          match
            ( record.admission.lifetime
            , String.equal record.admission.authority_sha256 authority_sha256 )
          with
          | Owned, true -> Ok ()
          | _ ->
            Error (unavailable Permission_denied "reserved parent authority has changed")
        in
        let diagnostics errors =
          unavailable
            Prompt_unavailable
            (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string
             |> String.concat ~sep:"\n")
        in
        let%bind definition =
          G.with_identity
            definition
            ~revision_id:record.admission.revision_id
            ~created_at:record.admission.created_at
          |> Result.map_error ~f:diagnostics
        in
        let%bind artifacts =
          Agent_store.Prompt_artifact_store.create
            ~env:t.env
            ~root:(Agent_store.Data_root.prompt_artifacts_path (S.data_root t.store))
          |> Result.map_error ~f:protocol_of_store
        in
        let%bind record =
          G.install_reserved
            ~delegations:ledger
            ~reservation:record
            ~artifact_store:artifacts
            definition
          |> Result.map_error ~f:diagnostics
        in
        let reference = D.reference record in
        let child_id = record.admission.child_session_id in
        let verify state =
          match state.State.spec.delegation with
          | Some actual when D.Reference.equal reference actual -> Ok ()
          | _ -> Error (corrupt "generated child does not match its creation reservation")
        in
        let%bind entry, fresh =
          match Session_registry.find t.registry child_id with
          | Some entry ->
            let%map () = A.state entry.actor |> Result.bind ~f:verify in
            entry, false
          | None ->
            let actor_lock_nonce =
              P.Id.Transaction.create () |> P.Id.Transaction.to_string
            in
            let%bind handle =
              match S.open_session t.store ~sw:t.sw ~actor_lock_nonce child_id with
              | Ok handle -> Ok handle
              | Error (Missing _)
                when D.equal_stage record.stage Child_installed
                     || D.equal_stage record.stage Linked ->
                Error
                  (unavailable
                     Session_not_found
                     "retained generated child is no longer available")
              | Error (Missing _) ->
                let%bind history, next =
                  G.initial_history definition ~session_id:child_id
                in
                let initial =
                  State.create
                    ~identity:
                      { session_id = child_id
                      ; display_name
                      ; creating_principal = Some principal_id
                      ; created_at = record.admission.created_at
                      ; updated_at = record.admission.created_at
                      ; labels = []
                      ; generation = 0
                      }
                    ~spec:
                      { before.spec with
                        protocol =
                          { protocol with
                            prompt = Generated record.admission.revision_id
                          }
                      ; prompt_definition_id = None
                      ; prompt_revision_id = record.admission.revision_id
                      ; delegation = Some reference
                      ; quota_key = None
                      }
                    ~initial_history:
                      (List.map history ~f:Agent_session.History_codec.to_protocol)
                in
                let initial =
                  { initial with
                    lifecycle = { desired = Stopped; observed = Stopped }
                  ; pending_initial_start = start_immediately
                  ; parent_stop_epoch =
                      Some (Option.value record.admission.parent_stop_epoch ~default:0L)
                  ; conversation =
                      { initial.conversation with
                        next_history_sequence = Int64.of_int next
                      ; reserved_history_through = Int64.of_int next
                      }
                  }
                in
                let%bind () = State.validate initial in
                S.create_session_initialized
                  t.store
                  ~sw:t.sw
                  ~transaction_id:record.admission.transaction_id
                  ~actor_lock_nonce
                  ~initialize:(initialize_generated_layout t initial)
                |> Result.map_error ~f:protocol_of_store
              | Error error -> Error (protocol_of_store error)
            in
            (match
               let%bind archived =
                 S.is_archived t.store handle |> Result.map_error ~f:protocol_of_store
               in
               let%bind () =
                 match archived with
                 | false -> Ok ()
                 | true ->
                   Error
                     (unavailable
                        Session_not_found
                        "retained generated child is archived")
               in
               let%bind initial = initial_recovery_state t handle in
               let%bind () = verify initial in
               let%map entry = recover_open_handle t handle in
               entry, true
             with
             | Ok _ as success -> success
             | Error _ as failure ->
               close_recovery_handle t handle;
               failure)
        in
        let publication () =
          let%bind () =
            Agent_store.Durable_file.sync_directory
              ~env:t.env
              ~path:(Agent_store.Data_root.sessions_path (S.data_root t.store))
            |> Result.map_error ~f:protocol_of_store
          in
          let%bind record =
            D.advance ledger record Child_installed
            |> Result.map_error ~f:protocol_of_store
          in
          A.checkpoint parent.actor ~persist:(fun current ->
            match check_parent current with
            | Ok () ->
              D.advance ledger record Linked
              |> Result.map ~f:ignore
              |> Result.map_error ~f:protocol_of_store
            | Error error ->
              let reason =
                match current.lifecycle.desired with
                | _ when not (Int64.equal current.stop_epoch before.stop_epoch) ->
                  D.Parent_stopped
                | Stopped -> D.Parent_stopped
                | Running -> Authority_changed
              in
              let%bind _ =
                D.revoke ledger record reason |> Result.map_error ~f:protocol_of_store
              in
              Error error)
        in
        let owned = ref fresh in
        Exn.protect
          ~finally:(fun () -> if !owned then entry.close ())
          ~f:(fun () ->
            let%bind () = publication () in
            let%bind () =
              match fresh with
              | false -> Ok ()
              | true -> Session_registry.add t.registry ~session_id:child_id entry
            in
            owned := false;
            let%map () = resume_generated_initial_start t entry in
            entry)))
  in
  with_generated_creation_lock t run
;;

let reconcile_generated_creations t =
  let module P = Agent_protocol in
  let module A = Agent_session.Session_actor in
  let module State = Agent_session.Session_state in
  let module Authority = Agent_session.Delegation_authority in
  let module G = Agent_session.Generated_definition in
  let module D = Agent_store.Delegation_store in
  let module S = Agent_store.Session_store in
  let module Artifacts = Agent_store.Prompt_artifact_store in
  let open Result.Let_syntax in
  let ledger = S.delegations t.store in
  let%bind records =
    D.with_records
      ledger
      ~max_records:t.limits.delegation_recovery_max_count
      ~max_bytes:t.limits.delegation_recovery_max_bytes
      ~f:(fun records -> Ok records)
    |> Result.map_error ~f:protocol_of_store
  in
  let revoke record reason =
    D.revoke ledger record reason
    |> Result.map ~f:ignore
    |> Result.map_error ~f:protocol_of_store
  in
  let native_capabilities (runtime : Agent_session.Runtime_builder.t) =
    match runtime.native_runtime with
    | None ->
      Error
        (unavailable Invalid_state "generated recovery needs qualified parent resources")
    | Some native ->
      Lazy.force native.capabilities
      |> Result.map_error ~f:(fun error ->
        unavailable Permission_denied error.Chat_response.Tool_capability.message)
  in
  let loaded id =
    Session_registry.find t.registry id
    |> Result.of_option
         ~error:(unavailable Permission_denied "delegation ancestor is unavailable")
  in
  let host : Authority.host =
    { state = (fun id -> Result.bind (loaded id) ~f:(fun entry -> A.state entry.actor))
    ; resolve =
        (fun reference ->
          D.resolve ledger reference |> Result.map_error ~f:protocol_of_store)
    ; capabilities =
        (fun id ->
          Result.bind (loaded id) ~f:(fun entry ->
            Runtime_owner.with_background_runtime entry.runtime native_capabilities))
    }
  in
  List.fold_result records ~init:() ~f:(fun () record ->
    let%bind () =
      D.discard_uninstalled_staging ledger record |> Result.map_error ~f:protocol_of_store
    in
    match record.D.stage, record.revocation with
    | Linked, _ | _, Some _ -> Ok ()
    | (Reserved | Artifact_installed | Child_installed), None ->
      (match Session_registry.load t.registry record.key.parent_session_id with
       | Error { code = Session_not_found; _ } -> revoke record Parent_deleted
       | Error _ as failure -> failure
       | Ok parent ->
         let%bind before = A.state parent.actor in
         (match
            ( before.lifecycle.desired
            , before.lifecycle.observed
            , before.halted
            , before.failure )
          with
          | _
            when not
                   (Int64.equal
                      before.stop_epoch
                      (Option.value record.admission.parent_stop_epoch ~default:0L)) ->
            revoke record Parent_stopped
          | Stopped, _, _, _ -> revoke record Parent_stopped
          | Running, _, true, _ | Running, _, _, Some _ -> revoke record Authority_changed
          | ( Running
            , ( Stopped
              | Queued_for_slot
              | Starting
              | Recovering
              | Compacting _
              | Stopping
              | Failed _ )
            , false
            , None ) -> Ok ()
          | Running, (Idle | Running_turn _ | Waiting_for_permission _), false, None ->
            let%bind fingerprint = Authority.fingerprint before in
            if not (String.equal fingerprint record.admission.authority_sha256)
            then revoke record Authority_changed
            else
              Runtime_owner.with_background_runtime parent.runtime (fun runtime ->
                let%bind current = native_capabilities runtime in
                let%bind artifacts =
                  Artifacts.create
                    ~env:t.env
                    ~root:
                      (Agent_store.Data_root.prompt_artifacts_path (S.data_root t.store))
                  |> Result.map_error ~f:protocol_of_store
                in
                match
                  Artifacts.exists artifacts record.admission.revision_id, record.stage
                with
                | false, Reserved -> Ok ()
                | false, _ ->
                  Error (corrupt "unfinished delegation lost its installed artifact")
                | true, _ ->
                  let diagnostics errors =
                    unavailable
                      Prompt_unavailable
                      (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string
                       |> String.concat ~sep:"\n")
                  in
                  let%bind _ =
                    G.load_artifact
                      ~artifact_store:artifacts
                      ~revision_id:record.admission.revision_id
                      ~manifest_sha256:record.admission.manifest_sha256
                    |> Result.map_error ~f:diagnostics
                  in
                  let%bind record =
                    D.advance ledger record Artifact_installed
                    |> Result.map_error ~f:protocol_of_store
                  in
                  let%bind () =
                    D.discard_uninstalled_staging ledger record
                    |> Result.map_error ~f:protocol_of_store
                  in
                  (match
                     Chat_response.Background_request.rebind_capabilities
                       ~pins:record.admission.capability_pins
                       ~capabilities:current
                   with
                   | Error _ -> revoke record Authority_changed
                   | Ok selected ->
                     let reference = D.reference record in
                     let child_id = record.admission.child_session_id in
                     let verify (state : State.t) =
                       match state.spec.delegation, state.lifecycle.desired with
                       | Some actual, Stopped when D.Reference.equal reference actual ->
                         Ok ()
                       | _ ->
                         Error
                           (corrupt
                              "unfinished generated child has an invalid identity or \
                               running state")
                     in
                     let%bind child =
                       match Session_registry.find t.registry child_id with
                       | Some entry -> Ok (Some (entry, false))
                       | None ->
                         (match
                            S.open_session
                              t.store
                              ~sw:t.sw
                              ~actor_lock_nonce:
                                (P.Id.Transaction.create () |> P.Id.Transaction.to_string)
                              child_id
                          with
                          | Error (Missing _) -> Ok None
                          | Error error -> Error (protocol_of_store error)
                          | Ok handle ->
                            let result =
                              let%bind archived =
                                S.is_archived t.store handle
                                |> Result.map_error ~f:protocol_of_store
                              in
                              match archived with
                              | true -> Ok None
                              | false ->
                                let%bind initial = initial_recovery_state t handle in
                                let%bind () = verify initial in
                                let%map entry = recover_open_handle t handle in
                                Some (entry, true)
                            in
                            (match result with
                             | Ok (Some _) -> result
                             | Ok None | Error _ ->
                               close_recovery_handle t handle;
                               result))
                     in
                     (match child with
                      | None ->
                        (match record.stage with
                         | Child_installed -> revoke record Admission_failed
                         | Reserved | Artifact_installed -> Ok ()
                         | Linked -> assert false)
                      | Some (child, fresh) ->
                        let owned = ref fresh in
                        Exn.protect
                          ~finally:(fun () -> if !owned then child.close ())
                          ~f:(fun () ->
                            let%bind state = A.state child.actor in
                            let%bind () = verify state in
                            let%bind _ =
                              G.restore
                                ~env:t.env
                                ~artifact_store:artifacts
                                ~revision_id:record.admission.revision_id
                                ~manifest_sha256:record.admission.manifest_sha256
                                ~current_capabilities:(fun () -> current)
                                ~pins:record.admission.capability_pins
                                ()
                              |> Result.map_error ~f:diagnostics
                            in
                            let authority =
                              Authority.create
                                ~max_depth:t.limits.delegation_max_depth
                                ~host
                                ~reference
                                ~capabilities:selected
                                ()
                            in
                            let%bind profile =
                              permission_profile_revision
                                t
                                state.spec.permission_profile_digest
                            in
                            let%bind () =
                              Authority.check_preparation
                                authority
                                ~session_id:child_id
                                ~revision_id:state.spec.prompt_revision_id
                                ~manifest_sha256:record.admission.manifest_sha256
                                ~permission_profile:profile
                            in
                            let%bind () =
                              Agent_store.Durable_file.sync_directory
                                ~env:t.env
                                ~path:
                                  (Agent_store.Data_root.sessions_path
                                     (S.data_root t.store))
                              |> Result.map_error ~f:protocol_of_store
                            in
                            let%bind record =
                              D.advance ledger record Child_installed
                              |> Result.map_error ~f:protocol_of_store
                            in
                            let%bind () =
                              A.checkpoint parent.actor ~persist:(fun latest ->
                                let%bind latest_fingerprint =
                                  Authority.fingerprint latest
                                in
                                match
                                  latest.lifecycle.desired, latest.halted, latest.failure
                                with
                                | Running, false, None
                                  when String.equal fingerprint latest_fingerprint
                                       && Int64.equal latest.stop_epoch before.stop_epoch
                                  ->
                                  D.advance ledger record Linked
                                  |> Result.map ~f:ignore
                                  |> Result.map_error ~f:protocol_of_store
                                | Stopped, _, _ -> revoke record Parent_stopped
                                | _
                                  when not
                                         (Int64.equal latest.stop_epoch before.stop_epoch)
                                  -> revoke record Parent_stopped
                                | _ -> revoke record Authority_changed)
                            in
                            let%map () =
                              match fresh with
                              | false -> Ok ()
                              | true ->
                                Session_registry.add t.registry ~session_id:child_id child
                            in
                            owned := false)))))))
;;
