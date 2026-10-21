open! Core
module P = Agent_protocol
module S = Agent_store.Session_store
module R = Agent_store.Developer_run_store
module B = Agent_store.Blob_store
module F = Developer_run_store_tests

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "store failure", (error : Agent_store.Store_error.t)]
;;

let document_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "document failure", (error : Document_schema.Error.t)]
;;

let store_limits () =
  let document =
    Document_schema.Limits.create
      ~max_bytes:4096
      ~max_depth:16
      ~max_fields:256
      ~max_nodes:512
    |> F.ok
  in
  R.Limits.create
    ~max_runs:3
    ~max_epochs_per_run:3
    ~max_jobs_per_run:4
    ~max_invocations_per_run:4
    ~max_inference_attempts_per_run:256
    ~commands:(Agent_store.Developer_command_limits.default ~document)
    ~max_inference_bytes_per_run:4096
    ~max_selected_per_commit:2
    ~max_job_payload_bytes:1024
    ~max_document_bytes:4096
    ~max_recovery_entries:16
    ~max_recovery_bytes:16384
    ~max_work_steps:200
    ~document
  |> Or_error.ok_exn
;;

let timestamp = P.Timestamp.of_string "2026-10-13T00:00:00Z" |> F.ok

let session_metadata () =
  let id = P.Id.Session.of_string "ses_developer_blob_custody" |> F.ok in
  let prompt = P.Id.Prompt_definition.of_string "prd_developer_blob_custody" |> F.ok in
  let workspace =
    P.Id.Workspace_definition.of_string "wsd_developer_blob_custody" |> F.ok
  in
  let spec =
    P.Session.Spec.create
      ~execution_host:Daemon
      ~prompt:(Catalog prompt)
      ~workspace:(Configured workspace)
      ~liveness:Detached
      ~persistence:Durable
      ~start_immediately:false
      ~display_name:"custody"
      ~labels:[]
      ()
    |> F.ok
  in
  let session =
    P.Session.
      { id
      ; creator = None
      ; created_at = timestamp
      ; updated_at = timestamp
      ; generation = 0
      ; spec
      ; desired_state = Stopped
      ; observed_state = Stopped
      ; prompt_revision = None
      ; workspace_instance = None
      ; active_operation = None
      ; revision = 0L
      ; metadata_revision = 0L
      ; organization = P.Session_organization.Values.empty
      ; latest_event_sequence = 0L
      ; inference_summary = History_entry.Payload.Presence.Absent
      }
  in
  S.Metadata.
    { schema_version = 1
    ; session
    ; prompt_artifact = "custody-prompt"
    ; workspace_identity = "custody-workspace"
    ; data_schema_version = 1
    }
;;

let session_job session =
  P.Job.
    { id = P.Id.Job.create ()
    ; session_id = S.Handle.session_id session
    ; generation = 0
    ; kind = Async_tool
    ; payload = `Null
    ; status = Running
    ; retry_policy = Never
    ; attempt = 1
    ; created_at = timestamp
    ; started_at = Some timestamp
    ; next_run_at = None
    ; completed_at = None
    ; result = None
    ; delivery = Pending
    ; launch = None
    ; progress = None
    }
;;

let with_custody f =
  F.with_temp_directory "developer-blob-custody" (fun env path ->
    Eio.Switch.run (fun sw ->
      let session =
        S.create
          ~env
          ~sw
          ~root:(Filename.concat path "data")
          ~server_id:F.server_id
          ~process_start_identity:None
          ~lock_nonce:"developer-blob-custody"
        |> ok
      in
      let runs =
        R.open_or_create session ~sw ~limits:(store_limits ()) ~checkpoint:ignore
        |> F.run_ok
      in
      let data_root = S.data_root session in
      let blobs =
        B.create
          ~env
          ~temporary_directory:(Agent_store.Data_root.temporary_blobs_path data_root)
          ~durable_directory:(Agent_store.Data_root.durable_blobs_path data_root)
          ~max_upload_bytes:4096L
        |> ok
      in
      B.bind_developer_runs blobs ~session_store:session runs |> ok;
      let result = f env sw session runs blobs in
      if not (R.is_closed runs) then R.close_and_wait runs |> F.run_ok;
      S.close session |> ok;
      result))
;;

let admit_root ?(owner = F.owner 0) runs =
  let handle, _ =
    R.admit_epoch
      runs
      ~owner
      ~capture_fingerprint:(F.digest "developer-blob-custody")
      ~previous:None
    |> F.run_ok
  in
  R.Result_root.of_handle handle |> F.run_ok |> B.Result_root.developer
;;

let publish_without_ack ~storage ~content ~sw ~runs ~blobs ~handle ~root ~observed ~job_id
  =
  let prepared =
    R.prepare_job_claim
      handle
      ~expected:observed
      ~lookup:(R.find_job_exact observed job_id)
      ~now:timestamp
    |> F.run_ok
  in
  let claimed = R.commit_job_claim handle prepared |> F.run_ok in
  let claim = R.Claimed.claim claimed in
  assert (
    R.Claim.try_enter claim ~handle ~admit:(fun () -> true) ~on_enter:(fun () -> ())
    |> F.run_ok);
  let state = P.Developer_job.state (R.Claim.job claim) in
  let proposed : P.Developer_job.State.t =
    { state with
      status = Succeeded
    ; completed_at = Some timestamp
    ; result = Some (Inline (P.Completion.Succeeded (`String content)))
    }
  in
  let limits =
    Agent_store.Developer_result_publisher.Limits.create
      ~inline_bytes:4096
      ~max_result_bytes:4096
      ~max_pending:1
    |> F.ok
  in
  let publisher =
    Agent_store.Developer_result_publisher.create ~sw ~runs ~root ~blobs ~limits |> F.ok
  in
  let selected =
    Agent_store.Developer_result_publisher.select
      publisher
      ~storage
      ~claim
      ~proposed
      ~selected_at:timestamp
    |> F.ok
  in
  Agent_store.Developer_result_publisher.Selected.ensure_attempted selected |> F.ok;
  let after = Agent_store.Developer_result_publisher.Selected.after selected in
  let transition =
    R.prepare_claimed_transition
      handle
      ~expected:(R.Claimed.observation claimed)
      ~claim
      ~after
      ~revoked_after:after
    |> F.run_ok
  in
  ignore (R.commit_claimed_transition handle transition |> F.run_ok : R.Installed.t);
  Agent_store.Developer_result_publisher.Selected.reference selected
;;

let stage root contents =
  let blob =
    P.Blob.Metadata.create
      ~id:(P.Id.Blob.create ())
      ~kind:File
      ~media_type:"text/plain"
      ~byte_length:(Int64.of_int (String.length contents))
      ~digest:Digestif.SHA256.(digest_string contents |> to_hex)
      ()
    |> F.ok
  in
  let metadata : B.Metadata.t =
    { blob
    ; creating_principal = F.principal_id
    ; target = B.Result_root.target root
    ; allowed_use = "developer result custody"
    ; created_at = P.Timestamp.of_string "2026-10-13T00:00:00Z" |> F.ok
    ; expires_at = None
    ; durable = false
    }
  in
  let document = Agent_store.Blob_metadata_document.create metadata |> document_ok in
  Agent_store.Blob_stage_documents.create document |> document_ok
;;

let%expect_test "developer result custody uses one verified root and reader budget" =
  with_custody (fun env sw session runs blobs ->
    let session_handle =
      S.create_session
        session
        ~sw
        ~transaction_id:(P.Id.Transaction.create ())
        ~actor_lock_nonce:"developer-custody-reader"
        (session_metadata ())
      |> ok
    in
    let root = admit_root runs in
    let contents = "owned result" in
    let stage = stage root contents in
    let read = B.ensure_result_staged_content blobs ~sw root ~stage contents |> ok in
    let id = (B.Result_read.metadata read).blob.id in
    let reopened = B.open_result blobs root id |> ok in
    let other_run_id = P.Id.Developer_run.of_string "dvr_other_blob_custody" |> F.ok in
    let other_root = admit_root ~owner:(F.owner ~run_id:other_run_id 0) runs in
    assert (Result.is_error (B.open_result blobs other_root id));
    let temporary = Agent_store.Data_root.temporary_blobs_path (S.data_root session) in
    let file suffix =
      Eio.Path.(
        Eio.Stdenv.fs env / Filename.concat temporary (P.Id.Blob.to_string id ^ suffix))
    in
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      (file ".sexp")
      (Agent_store.Blob_stage_documents.temporary_bytes stage);
    Eio.Path.save ~create:(`Exclusive 0o600) (file ".blob") contents;
    assert (Result.is_error (B.open_temporary blobs id));
    [%test_eq: string]
      contents
      (B.load_result_verified blobs ~sw reopened ~max_bytes:4096 |> ok);
    [%test_eq: string]
      "result"
      (B.read_result_range blobs ~sw reopened ~offset:6L ~max_bytes:6 |> ok);
    assert (
      Result.is_error (B.read_result_range blobs ~sw reopened ~offset:(-1L) ~max_bytes:6));
    assert (Result.is_error (B.load_result_verified blobs ~sw reopened ~max_bytes:4097));
    let reader =
      Agent_store.Retention_reader.create
        ~env
        ~root:(Agent_store.Data_root.path (S.data_root session))
        ~max_entries:100
        ~max_bytes:65536
      |> ok
    in
    let before = Agent_store.Retention_reader.remaining_bytes reader in
    (B.with_retention blobs ~f:(fun retention ->
       B.validate_developer_retention retention ~session:session_handle ~reader)
     |> ok
     |> Option.value_exn
     |> function
     | B.Has_developer_evidence -> ()
     | No_developer_evidence -> failwith "admitted run document was ignored");
    assert (Agent_store.Retention_reader.remaining_bytes reader < before);
    let limited =
      Agent_store.Retention_reader.create
        ~env
        ~root:(Agent_store.Data_root.path (S.data_root session))
        ~max_entries:100
        ~max_bytes:1
      |> ok
    in
    assert (
      Result.is_error
        (B.with_retention blobs ~f:(fun retention ->
           B.validate_developer_retention
             retention
             ~session:session_handle
             ~reader:limited)));
    let durable_run =
      Filename.concat
        (Filename.concat
           (Agent_store.Data_root.durable_blobs_path (S.data_root session))
           "developer-runs")
        (P.Id.Developer_run.to_string F.run_id)
    in
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      Eio.Path.(Eio.Stdenv.fs env / Filename.concat durable_run "foreign")
      "foreign";
    let fresh =
      Agent_store.Retention_reader.create
        ~env
        ~root:(Agent_store.Data_root.path (S.data_root session))
        ~max_entries:100
        ~max_bytes:65536
      |> ok
    in
    assert (
      Result.is_error
        (B.with_retention blobs ~f:(fun retention ->
           B.validate_developer_retention retention ~session:session_handle ~reader:fresh)));
    S.close_session session session_handle |> ok;
    [%test_eq: string]
      contents
      (B.load_result_verified blobs ~sw reopened ~max_bytes:4096 |> ok);
    print_endline "foreign run and generic temporary paths rejected developer metadata";
    print_endline "verified result read and exact range retained source bytes";
    print_endline "developer run documents and blob data charged the shared reader");
  [%expect
    {|
    foreign run and generic temporary paths rejected developer metadata
    verified result read and exact range retained source bytes
    developer run documents and blob data charged the shared reader
    |}]
;;

let%expect_test "closed developer issuer cannot lend a prior result read to a new store" =
  with_custody (fun _env sw session runs blobs ->
    let first_root = admit_root runs in
    let contents = "retained result" in
    let read =
      B.ensure_result_staged_content
        blobs
        ~sw
        first_root
        ~stage:(stage first_root contents)
        contents
      |> ok
    in
    R.close_and_wait runs |> F.run_ok;
    let reopened =
      R.open_or_create session ~sw ~limits:(store_limits ()) ~checkpoint:ignore
      |> F.run_ok
    in
    B.bind_developer_runs blobs ~session_store:session reopened |> ok;
    assert (Result.is_error (B.load_result_verified blobs ~sw read ~max_bytes:4096));
    let second_root =
      R.Result_root.find_existing reopened F.run_id
      |> F.run_ok
      |> Option.value_exn
      |> B.Result_root.developer
    in
    let id = (B.Result_read.metadata read).blob.id in
    let fresh = B.open_result blobs second_root id |> ok in
    [%test_eq: string]
      contents
      (B.load_result_verified blobs ~sw fresh ~max_bytes:4096 |> ok);
    R.close_and_wait reopened |> F.run_ok;
    print_endline "old issuer token rejected; new issuer reopened original verified bytes");
  [%expect {|old issuer token rejected; new issuer reopened original verified bytes|}]
;;

let%expect_test
    "scanned unrelated developer run permits collection; unbound evidence defers"
  =
  with_custody (fun env sw owner runs blobs ->
    let developer_owner = F.owner 0 in
    let capture = F.digest "developer-blob-custody" in
    let developer, original =
      R.admit_epoch
        runs
        ~owner:developer_owner
        ~capture_fingerprint:capture
        ~previous:None
      |> F.run_ok
    in
    let session =
      S.create_session
        owner
        ~sw
        ~transaction_id:(P.Id.Transaction.create ())
        ~actor_lock_nonce:"developer-retention-session"
        (session_metadata ())
      |> ok
    in
    let job = session_job session in
    let completion = P.Completion.Succeeded (`String (String.make 512 'r')) in
    let prepared =
      Agent_store.Job_result_store.prepare
        blobs
        ~env
        ~sw
        ~session
        ~job
        ~creating_principal:F.principal_id
        ~now:timestamp
        ~max_bytes:4096
        completion
      |> ok
    in
    let reference = Agent_store.Job_result_store.reference prepared in
    let retained_job = session_job session in
    let retained_prepared =
      Agent_store.Job_result_store.prepare
        blobs
        ~env
        ~sw
        ~session
        ~job:retained_job
        ~creating_principal:F.principal_id
        ~now:timestamp
        ~max_bytes:4096
        (P.Completion.Succeeded (`String "referenced across owners"))
      |> ok
    in
    let retained_reference = Agent_store.Job_result_store.reference retained_prepared in
    let reference_text = P.Id.Blob.to_string retained_reference.blob.id in
    let developer_job =
      let id = P.Id.Job.create () in
      let launch =
        P.Developer_job.Launch.create ~parent:Run_root ~parent_job:None ~nested_depth:0
        |> F.ok
      in
      let state : P.Developer_job.State.t =
        { status = Queued
        ; attempt = 0
        ; started_at = None
        ; next_run_at = None
        ; completed_at = None
        ; result = None
        ; retry_failure = None
        }
      in
      P.Developer_job.create
        ~owner:developer_owner
        ~capture_fingerprint:capture
        ~id
        ~launch
        ~kind:Async_tool
        ~payload:(`String reference_text)
        ~retry_policy:Never
        ~created_at:timestamp
        ~state
      |> F.ok
    in
    let selected =
      R.prepare_root_selected
        developer
        ~expected:original
        ~phase:R.Checkpoint.Active
        ~selected:[ developer_job ]
      |> F.run_ok
    in
    let handoff = R.Handoff.create ~settle:(fun _ -> ()) in
    let _selected_observed =
      match R.commit_and_settle developer selected handoff ~admit:(fun () -> true) with
      | Ok (R.Settlement.Installed installed) -> R.Installed.observation installed
      | Ok (Absent | Prewrite_conflict | Diverged) ->
        failwith "selected job not installed"
      | Error error -> raise_s [%sexp (error : R.Error.t)]
    in
    let authority =
      Eio.Path.(
        Eio.Stdenv.fs env
        / Agent_store.Data_root.path (S.data_root owner)
        / "developer-runs"
        / ("run_" ^ P.Id.Developer_run.to_string developer_owner.run_id ^ ".json"))
    in
    let original_bytes = Eio.Path.load authority in
    let escaped =
      "\\u"
      ^ sprintf "%04x" (Char.to_int reference_text.[0])
      ^ String.drop_prefix reference_text 1
    in
    let escaped_bytes =
      String.substr_replace_all original_bytes ~pattern:reference_text ~with_:escaped
    in
    assert (not (String.equal escaped_bytes original_bytes));
    Eio.Path.save ~create:(`Or_truncate 0o600) authority escaped_bytes;
    let marker =
      Filename.concat
        (Filename.concat (S.Handle.directory session) "result-preparations")
        (P.Id.Blob.to_string reference.blob.id ^ ".frame")
    in
    let publisher =
      Agent_store.Job_result_store.Publisher.create
        ~env
        ~blobs
        ~sw
        ~session
        ~principal:F.principal_id
        ~inline_bytes:0
        ~max_bytes:4096
      |> F.ok
    in
    let limits : Agent_store.Job_result_store.Publisher.collection_limits =
      { max_intents = 8
      ; max_entries = 100
      ; max_bytes = 65536
      ; max_file_bytes = 4096
      ; max_work = 100_000
      }
    in
    let collected =
      Agent_store.Job_result_store.Publisher.collect
        publisher
        ~jobs:[]
        ~generation:0
        ~limits
        ~with_roots:(fun ~reader:_ ~candidates:_ ~f -> f [])
    in
    let collected = ok collected |> Option.value_exn in
    [%test_eq: int] 1 collected.discarded;
    [%test_eq: int] 1 collected.retained;
    let cancellation = Failure "cancel scoped cross-owner proof" in
    (match
       Eio.Cancel.sub (fun cancel ->
         let reader =
           Agent_store.Retention_reader.create
             ~env
             ~root:(S.Handle.directory session)
             ~max_entries:100
             ~max_bytes:65536
           |> ok
         in
         B.with_collection_proof blobs ~f:(fun scope ->
           B.with_developer_retention
             scope
             ~session
             ~reader
             ~checkpoint:(fun () -> ())
             ~f:(fun _developer ->
               Eio.Cancel.cancel cancel cancellation;
               Eio.Fiber.check ();
               Ok ())))
     with
     | exception Eio.Cancel.Cancelled actual -> assert (phys_equal actual cancellation)
     | Ok _ | Error _ -> failwith "collection proof masked cancellation");
    let developer_root = R.Result_root.of_handle developer |> F.run_ok in
    let inline_reference =
      publish_without_ack
        ~storage:Automatic
        ~content:"developer published"
        ~sw
        ~runs
        ~blobs
        ~handle:developer
        ~root:developer_root
        ~observed:(R.observe developer |> F.run_ok)
        ~job_id:(P.Developer_job.id developer_job)
    in
    assert (Option.is_none inline_reference);
    let private_marker =
      Developer_intent_fixture.path
        (S.data_root owner)
        ~owner:developer_owner
        ~job:(P.Developer_job.id developer_job)
        ~attempt:1
    in
    assert (Developer_intent_fixture.present env private_marker);
    let second_collection =
      Agent_store.Job_result_store.Publisher.collect
        publisher
        ~jobs:[]
        ~generation:0
        ~limits
        ~with_roots:(fun ~reader:_ ~candidates:_ ~f -> f [])
      |> ok
      |> Option.value_exn
    in
    assert (second_collection.retired > 0);
    assert (not (Developer_intent_fixture.present env private_marker));
    let content_only_prepared =
      Agent_store.Job_result_store.prepare
        blobs
        ~env
        ~sw
        ~session
        ~job:(session_job session)
        ~creating_principal:F.principal_id
        ~now:timestamp
        ~max_bytes:4096
        (P.Completion.Succeeded (`String "Blob-content-only reference"))
      |> ok
    in
    let content_only_id =
      (Agent_store.Job_result_store.reference content_only_prepared).blob.id
    in
    let artifact_job_id = P.Id.Job.create () in
    let artifact_job =
      Developer_run_selected_tests.job
        developer_owner
        capture
        ~id:artifact_job_id
        ~parent:Run_root
    in
    let next_selected =
      R.prepare_root_selected
        developer
        ~expected:(R.observe developer |> F.run_ok)
        ~phase:R.Checkpoint.Active
        ~selected:[ artifact_job ]
      |> F.run_ok
    in
    let artifact_observed =
      match
        R.commit_and_settle
          developer
          next_selected
          (R.Handoff.create ~settle:(fun _ -> ()))
          ~admit:(fun () -> true)
      with
      | Ok (R.Settlement.Installed installed) -> R.Installed.observation installed
      | Ok (Absent | Prewrite_conflict | Diverged) ->
        failwith "artifact job not installed"
      | Error error -> raise_s [%sexp (error : R.Error.t)]
    in
    let artifact_reference =
      publish_without_ack
        ~storage:Artifact
        ~content:(P.Id.Blob.to_string content_only_id)
        ~sw
        ~runs
        ~blobs
        ~handle:developer
        ~root:developer_root
        ~observed:artifact_observed
        ~job_id:artifact_job_id
    in
    let artifact_reference = Option.value_exn artifact_reference in
    let artifact_marker =
      Developer_intent_fixture.path
        (S.data_root owner)
        ~owner:developer_owner
        ~job:artifact_job_id
        ~attempt:1
    in
    assert (Developer_intent_fixture.present env artifact_marker);
    let artifact_collection =
      Agent_store.Job_result_store.Publisher.collect
        publisher
        ~jobs:[]
        ~generation:0
        ~limits
        ~with_roots:(fun ~reader:_ ~candidates:_ ~f -> f [])
      |> ok
      |> Option.value_exn
    in
    assert (artifact_collection.retired > 0);
    assert (artifact_collection.retained >= 2);
    assert (Result.is_ok (B.open_session blobs session content_only_id));
    assert (not (Developer_intent_fixture.present env artifact_marker));
    let blocked_prepared =
      Agent_store.Job_result_store.prepare
        blobs
        ~env
        ~sw
        ~session
        ~job:(session_job session)
        ~creating_principal:F.principal_id
        ~now:timestamp
        ~max_bytes:4096
        (P.Completion.Succeeded (`String "must survive failed global proof"))
      |> ok
    in
    let blocked_id = (Agent_store.Job_result_store.reference blocked_prepared).blob.id in
    let blocked_marker =
      Filename.concat
        (Filename.concat (S.Handle.directory session) "result-preparations")
        (P.Id.Blob.to_string blocked_id ^ ".frame")
    in
    let exhausted =
      Agent_store.Job_result_store.Publisher.collect
        publisher
        ~jobs:[]
        ~generation:0
        ~limits:{ limits with max_work = 1 }
        ~with_roots:(fun ~reader:_ ~candidates:_ ~f -> f [])
    in
    assert (Result.is_error exhausted);
    assert (Eio.Path.is_file Eio.Path.(Eio.Stdenv.fs env / blocked_marker));
    let artifact_data =
      Eio.Path.(
        Eio.Stdenv.fs env
        / Agent_store.Data_root.durable_blobs_path (S.data_root owner)
        / "developer-runs"
        / P.Id.Developer_run.to_string developer_owner.run_id
        / (P.Id.Blob.to_string
             (Agent_store.Developer_result_reference.blob artifact_reference).id
           ^ ".blob"))
    in
    Eio.Path.save ~create:(`Or_truncate 0o600) artifact_data "corrupt";
    let corrupted =
      Agent_store.Job_result_store.Publisher.collect
        publisher
        ~jobs:[]
        ~generation:0
        ~limits
        ~with_roots:(fun ~reader:_ ~candidates:_ ~f -> f [])
    in
    assert (Result.is_error corrupted);
    assert (Eio.Path.is_file Eio.Path.(Eio.Stdenv.fs env / blocked_marker));
    R.close_and_wait runs |> F.run_ok;
    let unbound_collection =
      Agent_store.Job_result_store.Publisher.collect
        publisher
        ~jobs:[]
        ~generation:0
        ~limits
        ~with_roots:(fun ~reader:_ ~candidates:_ ~f -> f [])
    in
    assert (Result.is_error unbound_collection);
    let entered, signal_entered = Eio.Promise.create () in
    let release, signal_release = Eio.Promise.create () in
    let claimed, signal_claimed = Eio.Promise.create () in
    let claim_started, signal_claim_started = Eio.Promise.create () in
    Eio.Fiber.fork ~sw (fun () ->
      let held =
        S.Handle.with_developer_root_collection session ~f:(fun () ->
          Eio.Promise.resolve signal_entered ();
          Eio.Promise.await release;
          Ok ())
        |> ok
      in
      assert (Option.is_some held));
    Eio.Promise.await entered;
    let claim_completed = Atomic.make false in
    Eio.Fiber.fork ~sw (fun () ->
      Eio.Promise.resolve signal_claim_started ();
      let developer_owner = S.Developer_root.claim owner |> ok in
      Atomic.set claim_completed true;
      S.Developer_root.release developer_owner;
      Eio.Promise.resolve signal_claimed ());
    Eio.Promise.await claim_started;
    Eio.Fiber.yield ();
    assert (not (Atomic.get claim_completed));
    Eio.Promise.resolve signal_release ();
    Eio.Promise.await claimed;
    assert (not (Eio.Path.is_file Eio.Path.(Eio.Stdenv.fs env / marker)));
    assert (Result.is_error (B.open_session blobs session reference.blob.id));
    assert (Result.is_ok (B.open_session blobs session retained_reference.blob.id));
    S.close_session owner session |> ok;
    print_endline
      "escaped developer job reference retained its candidate; unrelated candidate \
       deleted; unbound run evidence blocked collection";
    print_endline "collection gate excluded a new developer root claim until release");
  [%expect
    {|
    escaped developer job reference retained its candidate; unrelated candidate deleted; unbound run evidence blocked collection
    collection gate excluded a new developer root claim until release
    |}]
;;

let%test_unit "an intent entry without a run document blocks conversation collection" =
  with_custody (fun env sw owner runs blobs ->
    let session =
      S.create_session
        owner
        ~sw
        ~transaction_id:(P.Id.Transaction.create ())
        ~actor_lock_nonce:"developer-intent-presence"
        (session_metadata ())
      |> ok
    in
    let job = session_job session in
    let prepared =
      Agent_store.Job_result_store.prepare
        blobs
        ~env
        ~sw
        ~session
        ~job
        ~creating_principal:F.principal_id
        ~now:timestamp
        ~max_bytes:4096
        (P.Completion.Succeeded `Null)
      |> ok
    in
    let reference = Agent_store.Job_result_store.reference prepared in
    let publisher =
      Agent_store.Job_result_store.Publisher.create
        ~env
        ~blobs
        ~sw
        ~session
        ~principal:F.principal_id
        ~inline_bytes:0
        ~max_bytes:4096
      |> F.ok
    in
    R.close_and_wait runs |> F.run_ok;
    let directory =
      Agent_store.Data_root.developer_result_intents_path (S.data_root owner)
    in
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      Eio.Path.(Eio.Stdenv.fs env / Filename.concat directory "orphan.frame")
      "unscanned";
    let limits : Agent_store.Job_result_store.Publisher.collection_limits =
      { max_intents = 8
      ; max_entries = 100
      ; max_bytes = 65536
      ; max_file_bytes = 4096
      ; max_work = 100_000
      }
    in
    assert (
      Result.is_error
        (Agent_store.Job_result_store.Publisher.collect
           publisher
           ~jobs:[]
           ~generation:0
           ~limits
           ~with_roots:(fun ~reader:_ ~candidates:_ ~f -> f [])));
    let marker =
      Filename.concat
        (Filename.concat (S.Handle.directory session) "result-preparations")
        (P.Id.Blob.to_string reference.blob.id ^ ".frame")
    in
    assert (Eio.Path.is_file Eio.Path.(Eio.Stdenv.fs env / marker));
    S.close_session owner session |> ok)
;;
