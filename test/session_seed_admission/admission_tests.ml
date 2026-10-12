open! Core
module P = Agent_protocol
module S = Agent_session
module D = Document_schema
module A = Session_seed_admission
module State = S.Session_state

let ok = function
  | Ok value -> value
  | Error error -> failwith error.P.Error.message
;;

let store_ok result =
  Result.map_error result ~f:(fun error ->
    Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let doc_ok result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let limits = Agent_store.Document_fields.limits ~max_bytes:(16 * 1024 * 1024) |> doc_ok
let timestamp = P.Timestamp.of_string "2026-10-11T12:13:14.123456789Z" |> ok
let prompt_id = P.Id.Prompt_definition.of_string "prd_seed" |> ok
let revision_id = P.Id.Prompt_revision.of_string "prv_seed" |> ok
let workspace_id = P.Id.Workspace_definition.of_string "wsd_seed" |> ok
let instance_id = P.Id.Workspace_instance.of_string "wsi_seed" |> ok
let session_id = P.Id.Session.of_string "ses_seed" |> ok

let definition =
  S.Prompt_definition.create
    ~id:prompt_id
    ~config_name:"seed"
    ~root_file:"/root.chatmd"
    ~root_invocation:Headless
    ~allowed_workspaces:[ workspace_id ]
    ~permission_profile:"seed-profile"
    ~runtime_policy:(Some "retained-policy")
    ~enabled:true
    ~description:None
  |> store_ok
;;

let workspace =
  S.Workspace_definition.create
    ~id:workspace_id
    ~config_name:"seed-workspace"
    ~source:(Physical { configured_root = "/workspace" })
    ~access:Read_only
    ~conflict_domain:None
    ~prompt_limits:[]
  |> store_ok
;;

let profile =
  S.Permission_policy.create
    ~id:"seed-profile"
    ~tool_default:Deny
    ~approval_timeout_ms:None
    ~fallback:Fallback_deny
    ~manifest_authorization:Deny_manifest
    ~evaluator:None
    ~evaluator_revision:None
    ~reviewer:None
  |> ok
;;

let target =
  Inference.Request.Target.create
    ~adapter:"fixture"
    ~profile:"fixed-profile"
    ~profile_revision:None
    ~account:None
    ~endpoint:"fixture"
    ~model:"fixed-model"
    ~settings:[]
    ~limits
  |> Result.map_error ~f:(fun error ->
    Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let selection =
  Inference.Selection.captured target ~limits
  |> Result.map_error ~f:(fun error ->
    Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let protocol =
  P.Session.Spec.create
    ~execution_host:Daemon
    ~prompt:(Catalog prompt_id)
    ~workspace:(Configured workspace_id)
    ~liveness:Detached
    ~persistence:Durable
    ~start_immediately:false
    ~labels:[ "opaque", "a\"b\n" ]
    ()
  |> ok
;;

let identity =
  State.Identity.
    { session_id
    ; display_name = None
    ; creating_principal = Some (P.Id.Principal.of_string "pri_seed" |> ok)
    ; created_at = timestamp
    ; updated_at = timestamp
    ; labels = protocol.labels
    ; generation = 0
    ; metadata_revision = 0L
    ; organization = P.Session_organization.Values.empty
    }
;;

let actual_workspace =
  S.Workspace_instance.
    { id = instance_id
    ; definition_id = Some workspace_id
    ; source_kind = Physical
    ; configured_root = Some "/workspace"
    ; canonical_root = { native_path = "/workspace"; device = 1L; inode = 2L }
    ; conflict_domain = "fs:1:2"
    ; access = Read_only
    ; cleanup = None
    ; server_created = false
    ; created_at = timestamp
    ; cleanup_completion = None
    }
;;

let spec =
  State.Spec.
    { protocol
    ; prompt_definition_id = Some prompt_id
    ; root_invocation = Headless
    ; prompt_revision_id = revision_id
    ; configuration_revision = 4L
    ; inference_target = selection
    ; delegation = None
    ; workspace_instance = actual_workspace
    ; permission_profile = profile.id
    ; permission_profile_digest = profile.revision_digest
    ; runtime_policy = definition.runtime_policy
    ; quota_key =
        Some { S.Quota_key.conflict_domain = actual_workspace.conflict_domain; prompt_id }
    }
;;

let retained_state = State.create ~identity ~spec ~initial_history:[]

let expected =
  P.Session_lifecycle.Expected.create
    ~reference:
      (P.Session_ref.create
         ~server_id:(P.Id.Server.of_string "srv_seed" |> ok)
         ~session_id)
    ~generation:0
    ~session_revision:0L
    ~lifecycle_revision:P.Session_lifecycle.Revision.zero
  |> ok
;;

let request =
  let transcript =
    P.Session_seed.Transcript.create
      []
      ~next_history_sequence:0L
      ~limits
      ~max_entries:1
      ~max_work_steps:1000
      ~checkpoint:Fn.ignore
    |> ok
  in
  P.Session_seed.Request.create
    protocol
    ~code:(Retained expected)
    ~transcript
    ~idempotency_key:(P.Idempotency_key.of_string "seed-command" |> ok)
  |> ok
;;

let recipe
      ?(state = retained_state)
      ?(definition = definition)
      ?(profile = profile)
      checkpoint
  =
  A.retained request ~state ~definition ~workspace ~current_profile:profile ~checkpoint
;;

let audit =
  Agent_store.Idempotency_store.Command_audit.encode
    { key =
        { principal_id = P.Id.Principal.of_string "pri_seed" |> ok
        ; session_id = None
        ; method_name = "session.seed"
        ; idempotency_key = P.Session_seed.Request.idempotency_key request
        }
    ; request_digest = String.make 64 'a'
    ; protected_record = true
    }
  |> store_ok
;;

let projection =
  S.Workspace_instance.creation_projection
    workspace
    ~instance_id
    ~configured_root:"/workspace"
    ~canonical_root:"/workspace"
    ~created_at:timestamp
;;

let prelayout ?(limits = limits) ?(projection = projection) checkpoint =
  A.Prelayout.admit
    (recipe Fn.ignore |> ok)
    ~identity
    ~workspace_instance_id:instance_id
    ~workspace_projection:projection
    ~command_audit:audit
    ~limits
    ~max_work_steps:100000
    ~checkpoint
;;

let%test_unit "retained admission preserves code and rejects current policy drift" =
  assert (Result.is_ok (recipe Fn.ignore));
  let changed =
    { profile with S.Permission_policy.revision_digest = String.make 64 'f' }
  in
  assert (Result.is_error (recipe ~profile:changed Fn.ignore));
  let definition =
    S.Prompt_definition.create
      ~id:prompt_id
      ~config_name:"seed"
      ~root_file:"/changed.chatmd"
      ~root_invocation:Ui
      ~allowed_workspaces:[ workspace_id ]
      ~permission_profile:profile.id
      ~runtime_policy:None
      ~enabled:true
      ~description:None
    |> store_ok
  in
  assert (Result.is_error (recipe ~definition Fn.ignore));
  let unresolved =
    Inference.Selection.unresolved ~limits
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  assert (
    Result.is_error
      (recipe
         ~state:{ retained_state with spec = { spec with inference_target = unresolved } }
         Fn.ignore))
;;

let%test_unit "shared native field builder and structural emitters preserve ordinary wire"
  =
  let raw = State.create_fields ~identity ~spec ~desired:Stopped ~initial_history:[] in
  assert (
    String.equal
      (Sexp.to_string (State.sexp_of_t raw))
      (Sexp.to_string (State.sexp_of_t retained_state)));
  let native =
    S.Session_state_document.encode
      (S.Session_state_document.authored retained_state)
      ~limits
    |> doc_ok
  in
  let payload =
    S.Session_state_document.project_fields
      raw
      ~spec:(fun spec ->
        S.Session_state_document.project_spec
          spec
          ~workspace:S.Workspace_instance.to_jsonaf)
      ~limits
    |> ok
  in
  assert (Jsonaf.exactly_equal payload (D.Document.payload native));
  let created =
    S.Session_delta_document.create
      (Created retained_state)
      ~limits
      ~state_document:S.Session_state_document.authored
    |> doc_ok
    |> S.Session_delta_document.document
  in
  let projected = S.Session_delta_document.project_created native ~limits |> doc_ok in
  assert (String.equal (D.Document.to_string created) (D.Document.to_string projected));
  assert (
    Jsonaf.exactly_equal
      (P.Session.to_json (State.summary retained_state))
      (P.Session.to_json
         (State.summary_fields raw ~workspace:(fun workspace ->
            workspace.S.Workspace_instance.id))))
;;

let%expect_test "prelayout recipe remains stopped and pending until explicit start" =
  let admitted = prelayout Fn.ignore |> ok in
  let state =
    A.Prelayout.state admitted ~workspace:actual_workspace ~checkpoint:Fn.ignore |> ok
  in
  assert (Inference.Selection.equal state.spec.inference_target selection);
  assert (P.Id.Prompt_revision.equal state.spec.prompt_revision_id revision_id);
  assert (
    List.is_empty state.attachments
    && List.is_empty state.permissions
    && List.is_empty state.invocations);
  print_s
    [%sexp
      (state.lifecycle : State.Lifecycle.t)
    , (state.runtime_initialization : State.Runtime_initialization.t)];
  [%expect {| (((desired Stopped) (observed Stopped)) (Pending (fresh_history false))) |}]
;;

let byte_limits bytes =
  D.Limits.create
    ~max_bytes:bytes
    ~max_depth:(D.Limits.max_depth limits)
    ~max_fields:(D.Limits.max_fields limits)
    ~max_nodes:(D.Limits.max_nodes limits)
  |> doc_ok
;;

let actual_transaction state =
  let delta =
    S.Session_delta_document.create
      (Created state)
      ~limits
      ~state_document:S.Session_state_document.authored
    |> doc_ok
    |> S.Session_delta_document.document
  in
  let event =
    P.Event.Durable.of_payload
      ~session_id
      ~sequence:1L
      ~revision:1L
      ~timestamp
      (Session_created (State.summary state))
    |> fun event ->
    S.Durable_event_document.create event ~limits
    |> doc_ok
    |> S.Durable_event_document.document
  in
  Agent_store.Transaction.create
    ~limits
    ~session_id
    ~generation:0
    ~transaction_sequence:1L
    ~previous_transaction_hash:None
    ~session_revision:1L
    ~first_event_sequence:(Some 1L)
    ~last_event_sequence:(Some 1L)
    ~accepted_at_ns:
      (Time_ns.to_int63_ns_since_epoch (P.Timestamp.to_time_ns timestamp)
       |> Int63.to_int64)
    ~command_audit:(Some audit)
    ~delta
    ~durable_events:[ event ]
  |> store_ok
;;

let%test_unit
    "prelayout whole-record boundary bounds real Created variants and every scalar"
  =
  (* Independent byte oracle: actual compact first transaction, not a replica of
     Prelayout's structural projection or its accounting traversal. *)
  let rec first_admitted low high =
    if low = high
    then low
    else (
      let middle = low + ((high - low) / 2) in
      if Result.is_ok (prelayout ~limits:(byte_limits middle) Fn.ignore)
      then first_admitted low middle
      else first_admitted (middle + 1) high)
  in
  let threshold = first_admitted 1 (D.Limits.max_bytes limits) in
  assert (threshold > 1);
  assert (Result.is_error (prelayout ~limits:(byte_limits (threshold - 1)) Fn.ignore));
  let admitted = prelayout ~limits:(byte_limits threshold) Fn.ignore |> ok in
  List.iter
    [ 1L, 2L; Int64.max_value, Int64.max_value; 0L, 0L ]
    ~f:(fun (device, inode) ->
      let workspace =
        { actual_workspace with
          canonical_root = { actual_workspace.canonical_root with device; inode }
        ; conflict_domain = sprintf "fs:%Ld:%Ld" device inode
        }
      in
      let state = A.Prelayout.state admitted ~workspace ~checkpoint:Fn.ignore |> ok in
      let encoded = Agent_store.Transaction.encode (actual_transaction state) in
      assert (String.length encoded <= threshold));
  let too_long =
    { actual_workspace with
      canonical_root =
        { actual_workspace.canonical_root with native_path = "/workspace-longer" }
    }
  in
  (* Even though smaller actual device/inode spellings leave total byte room,
     a longer path is outside the prelayout component proof and must reject. *)
  assert (
    String.length (Jsonaf.to_string (S.Workspace_instance.to_jsonaf too_long))
    < String.length (Jsonaf.to_string projection));
  assert (
    Result.is_error (A.Prelayout.state admitted ~workspace:too_long ~checkpoint:Fn.ignore))
;;

exception Injected of int

let%test_unit "every observed prelayout interruption preserves input and subsequent reuse"
  =
  let calls = ref 0 in
  let admitted = prelayout (fun () -> Int.incr calls) |> ok in
  ignore
    (A.Prelayout.state admitted ~workspace:actual_workspace ~checkpoint:Fn.ignore |> ok);
  assert (!calls > 0);
  let original = Jsonaf.to_string projection in
  for ordinal = 1 to !calls do
    let seen = ref 0 in
    let sentinel = Injected ordinal in
    let outcome =
      try
        ignore
          (prelayout (fun () ->
             Int.incr seen;
             if !seen = ordinal then raise sentinel));
        false
      with
      | exn -> phys_equal exn sentinel
    in
    assert outcome;
    assert (String.equal original (Jsonaf.to_string projection))
  done;
  ignore (prelayout Fn.ignore |> ok)
;;
