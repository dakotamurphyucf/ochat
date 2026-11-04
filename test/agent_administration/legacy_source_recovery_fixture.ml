open! Core
module T = Agent_administration_test
module P = Agent_protocol
module A = Agent_session.Session_actor
module State = Agent_session.Session_state
module Revision = Agent_session.Prompt_revision
module Artifacts = Agent_store.Prompt_artifact_store
module Store = Agent_store.Session_store
module Persistence = Agent_session.Session_persistence
module Runtime = Agent_server.Runtime_owner
module CM = Prompt.Chat_markdown
module D = Document_schema
module State_document = Agent_session.Session_state_document

module Preservation = struct
  type t =
    | Known
    | Protected_unknown
end

let ok = T.ok

let stored = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
;;

let document_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : D.Error.t)]
;;

let checkpoint_in_document document =
  match D.Json.field (D.Document.payload document) ~name:"moderator" with
  | Value value -> value
  | Absent | Null -> failwith "prior checkpoint is missing from complete document"
;;

let assert_preservation preservation checkpoint =
  match preservation, D.Json.field checkpoint ~name:"prior_compatible" with
  | Preservation.Known, Absent -> ()
  | Protected_unknown, Value (`String value) -> assert (String.equal value "keep me")
  | Known, (Value _ | Null) | Protected_unknown, (Value _ | Absent | Null) ->
    failwith "prior checkpoint preservation differs"
;;

let stored_checkpoint ~env (entry : Agent_server.Session_registry.entry) =
  let handle = Option.value_exn entry.store_handle in
  let installed =
    Agent_store.Snapshot.load_current
      ~env
      ~directory:(Store.Handle.snapshot_directory handle)
      ~max_payload_length:
        Agent_server.Daemon.default_options.factory_limits.snapshot_payload_limit
    |> stored
    |> Option.value_exn
  in
  checkpoint_in_document installed.snapshot.payload
;;

let archived_checkpoint ~env (entry : Agent_server.Session_registry.entry) reference =
  let handle = Option.value_exn entry.store_handle in
  let max_payload_length =
    Agent_server.Daemon.default_options.factory_limits.snapshot_payload_limit
  in
  let contents =
    Agent_store.Durable_file.load_bounded
      ~env
      ~path:
        (Filename.concat
           (Store.Handle.archive_directory handle)
           (Agent_session.Compaction_archive.filename reference))
      ~max_bytes:(max_payload_length + 52)
    |> stored
  in
  let _, document =
    Agent_session.Compaction_archive.decode_record
      ~max_payload_length
      ~expected_digest:(Some reference.sha256)
      contents
    |> ok
  in
  let document =
    State_document.encode document ~limits:Document_schema.Limits.default
    |> function
    | Ok value -> value
    | Error error -> raise_s [%sexp (error : D.Error.t)]
  in
  checkpoint_in_document document
;;

type t =
  { state : State.t
  ; document : State_document.t
  ; artifact : Artifacts.Artifact.t
  ; server_id : P.Id.Server.t
  ; principal : P.Principal.t
  ; canonical_sha256 : string
  ; preservation : Preservation.t
  }

let legacy_script revision =
  List.fold (Revision.elements revision) ~init:None ~f:(fun selected -> function
    | CM.Script script -> Some script
    | _ -> selected)
  |> Option.value_exn
;;

let prior_edition daemon principal session_id desired ~preservation =
  let _, current = T.state daemon session_id in
  let revision =
    Agent_session.Prompt_catalog.find_revision
      (Agent_server.Daemon.prompts daemon)
      current.spec.prompt_revision_id
    |> Option.value_exn
  in
  let retained = Revision.artifact revision in
  let script = legacy_script revision in
  let source =
    match script.source with
    | Inline source -> source
    | Src { source_text; _ } -> source_text
  in
  (* Construct a prior storage edition with public data constructors. This is
     not a current source/native receipt and grants no execution authority. *)
  let artifact =
    Artifacts.Artifact.create
      ~revision_id:(P.Id.Prompt_revision.create ())
      ?prompt_definition_id:retained.prompt_definition_id
      ~root_relative_path:retained.root_relative_path
      ~root_chatmd:retained.root_chatmd
      ~sources:retained.sources
      ~parser_schema_version:1
      ~runtime_schema_version:retained.runtime_schema_version
      ~created_at:retained.created_at
      ()
    |> stored
  in
  let snapshot =
    Agent_session.Moderator_checkpoint.decode current.moderator |> ok |> Option.value_exn
  in
  let snapshot =
    { snapshot with
      script_id = script.id
    ; script_source_hash = Md5.digest_string source |> Md5.to_hex
    }
  in
  let moderator = Agent_session.Moderator_checkpoint.encode snapshot in
  let initial =
    State.create
      ~identity:current.identity
      ~spec:{ current.spec with prompt_revision_id = artifact.revision_id }
      ~initial_history:[]
  in
  let now = initial.identity.updated_at in
  let job =
    P.Job.
      { id = P.Id.Job.of_string "job_prior_queued" |> ok
      ; session_id
      ; generation = initial.identity.generation
      ; kind = Async_tool
      ; payload = `Object []
      ; status = Queued
      ; retry_policy = Never
      ; attempt = 0
      ; created_at = now
      ; started_at = None
      ; next_run_at = None
      ; completed_at = None
      ; result = None
      ; delivery = Not_required
      ; launch = None
      ; inference_origin = None
      ; progress = None
      }
  in
  let schedule =
    P.Schedule.
      { id = P.Id.Schedule.of_string "sch_prior_due" |> ok
      ; session_id
      ; generation = initial.identity.generation
      ; payload = `Object []
      ; created_at = now
      ; next_due_at = now
      ; misfire = Deliver_once_immediately
      ; status = Scheduled
      ; delivery_count = 0
      ; last_delivery_at = None
      ; ownership = None
      ; inference_provenance = None
      ; delivery_cancellation = None
      }
  in
  let state =
    { initial with
      lifecycle =
        { desired
        ; observed =
            (match desired with
             | Stopped -> Stopped
             | Running -> Idle)
        }
    ; runtime_initialization = Ready
    ; moderator = Some moderator
    ; jobs = [ job ]
    ; schedules = [ schedule ]
    }
  in
  State.validate state |> ok;
  (* Unknown historical fields belong to a decoded preservation carrier, not
     to the native known-value encoder. Start with the valid known state, admit
     the constructed prior raw document, then retain that exact owner. *)
  let limits = D.Limits.default in
  let known =
    State_document.encode (State_document.authored state) ~limits |> document_ok
  in
  let raw_payload =
    match preservation, D.Document.payload known, moderator with
    | Preservation.Known, payload, _ -> payload
    | Protected_unknown, `Object fields, `Object moderator_fields ->
      let raw_moderator =
        `Object (("prior_compatible", `String "keep me") :: moderator_fields)
      in
      `Object
        (List.map fields ~f:(fun (name, value) ->
           name, if String.equal name "moderator" then raw_moderator else value))
    | _ -> assert false
  in
  let raw_document =
    D.Document.create
      ~limits
      ~kind:(D.Document.kind known)
      ~version:(D.Document.version known)
      ~payload:raw_payload
    |> document_ok
  in
  let document = State_document.decode ~limits raw_document |> document_ok in
  { state = State_document.value document
  ; document
  ; artifact
  ; server_id = Store.server_id (Agent_server.Daemon.store daemon)
  ; principal
  ; canonical_sha256 = Chatmd_shell_spec.Source_ref.digest source
  ; preservation
  }
;;

(* Match Factory.initialize_generated_layout: no published metadata/index row
   precedes the complete private first Created transaction and snapshot. *)
let initialize_prior_layout ~sw ~env document ~staging_directory =
  let open Result.Let_syntax in
  let state = State_document.value document in
  let limits = Agent_server.Daemon.default_options.factory_limits in
  let%bind journal =
    Agent_store.Journal.create
      ~env
      ~directory:(Filename.concat staging_directory "journal")
      ~max_payload_length:limits.max_journal_payload
      ~max_segment_bytes:limits.max_segment_bytes
      ~max_segment_frames:limits.max_segment_frames
  in
  let%bind writer =
    Agent_store.Commit_writer.create
      ~sw
      ~journal
      ~session_id:state.identity.session_id
      ~next_transaction_sequence:1L
      ~previous_transaction_hash:None
      ~queue_capacity:8
  in
  Exn.protect
    ~finally:(fun () -> Agent_store.Commit_writer.close writer)
    ~f:(fun () ->
      let persistence =
        Persistence.create
          ~archive:(fun _ _ -> failwith "prior Created must not archive")
          ~pending_archive:None
          ~before_commit:None
          ~command_accepted:(fun _ _ -> ())
          ~writer
          ~durability:Flush
          ~limits:Document_schema.Limits.default
          ~archive_limits:Document_schema.Limits.default
          ~retention_preflight:None
          ~restored:(Persistence.Restored.of_state_document document)
          ~previous_transaction_hash:None
      in
      let store_error (error : P.Error.t) =
        Agent_store.Store_error.Corrupt error.message
      in
      let%bind transition =
        Agent_session.Session_transition.apply
          ~now:state.identity.updated_at
          state
          ~delta:(Created state)
          ~payloads:[ Session_created (State.summary state) ]
        |> Result.map_error ~f:store_error
      in
      let%bind () =
        Persistence.commit persistence ~command_audit:None ~previous:state transition
        |> Result.map_error ~f:store_error
      in
      let%bind _ =
        Persistence.install_snapshot_at
          persistence
          ~env
          ~directory:(Filename.concat staging_directory "snapshot")
          ~max_payload_length:limits.snapshot_payload_limit
          ~transaction_hash:(Persistence.transaction_hash persistence)
          transition.state
      in
      let%bind () =
        Agent_store.Durable_file.sync_directory ~env ~path:staging_directory
      in
      Store.Initial_projection.create
        ~metadata:(Agent_server.Session_projection.metadata transition.state)
        ~entry:(Agent_server.Session_projection.index_entry transition.state))
;;

let install_prior ~sw ~env ~data_dir prior =
  let store =
    Store.create
      ~env
      ~sw
      ~root:data_dir
      ~server_id:prior.server_id
      ~process_start_identity:None
      ~lock_nonce:"prior-edition-store"
    |> stored
  in
  Exn.protect
    ~finally:(fun () -> Store.close store |> stored)
    ~f:(fun () ->
      let artifacts =
        Artifacts.open_existing
          ~env
          ~root:(Agent_store.Data_root.prompt_artifacts_path (Store.data_root store))
        |> stored
      in
      Artifacts.install
        artifacts
        ~transaction_id:(P.Id.Transaction.create ())
        prior.artifact
      |> stored;
      let handle =
        Store.create_session_initialized
          store
          ~sw
          ~transaction_id:(P.Id.Transaction.create ())
          ~actor_lock_nonce:"prior-edition-session"
          ~initialize:(initialize_prior_layout ~sw ~env prior.document)
        |> stored
      in
      Exn.protect
        ~finally:(fun () -> Store.close_session store handle |> stored)
        ~f:(fun () ->
          let installed =
            Agent_store.Snapshot.load_current
              ~env
              ~directory:(Store.Handle.snapshot_directory handle)
              ~max_payload_length:
                Agent_server.Daemon.default_options.factory_limits.snapshot_payload_limit
            |> stored
            |> Option.value_exn
          in
          checkpoint_in_document installed.snapshot.payload))
;;

let assert_queued_unchanged (state : State.t) =
  let job = List.hd_exn state.jobs in
  let timer = List.hd_exn state.schedules in
  (match job.status, timer.status with
   | Queued, Scheduled -> ()
   | _ ->
     raise_s
       [%sexp "migration changed queued work", (job : P.Job.t), (timer : P.Schedule.t)]);
  assert (job.attempt = 0);
  assert (timer.delivery_count = 0)
;;

let assert_blocked_claims entry =
  let state = A.state entry.Agent_server.Session_registry.actor |> ok in
  let before = state.counters.revision in
  let job = List.hd_exn state.jobs
  and timer = List.hd_exn state.schedules in
  let denied = function
    | Error error -> assert (P.Error.equal_code error.P.Error.code Migration_required)
    | Ok _ -> failwith "migration-blocked claim succeeded"
  in
  A.claim_job entry.actor ~job_id:job.id ~generation:job.generation |> denied;
  A.claim_schedule entry.actor ~schedule_id:timer.id ~generation:timer.generation
  |> denied;
  let _, due = A.due_schedules entry.actor |> ok in
  assert (List.is_empty due);
  let after = A.state entry.actor |> ok in
  assert (Int64.equal before after.counters.revision);
  assert_queued_unchanged after
;;
