open! Core
open Agent_server_test_support
open Retry_support
module Birth = Descendant_goal_birth_tests
module Historical = Child_usage_fault_tests.Historical_unrelated
module Existing = Source_preparation_tests
module S = Agent_store
module D = S.Delegation_store
module J = Document_schema
module Origin = S.Delegation_goal_origin

let generated = function
  | Ok value -> value
  | Error diagnostics ->
    failwith
      (List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string
       |> String.concat ~sep:"\n")
;;

let resolve daemon reference =
  D.resolve (S.Session_store.delegations (Daemon.store daemon)) reference
  |> Existing.store_ok
;;

let document value = J.Document.json value

let ledger (current : A.Session_state.t) =
  match A.Inference_ledger.to_document current.inference_ledger with
  | Ok value -> document value
  | Error error -> raise_s [%sexp (error : A.Inference_ledger.Error.t)]
;;

let accounting (current : A.Session_state.t) =
  Option.map current.goal_descendants ~f:(fun value ->
    A.Goal_descendants.to_document value |> protocol_ok |> document)
;;

let runs (current : A.Session_state.t) =
  Option.value_map current.run_state ~default:[] ~f:A.Run_state.runs
  |> List.map ~f:P.Run.to_json
  |> fun values -> `Array values
;;

let assert_origin record expected =
  assert (Option.equal Origin.equal record.D.admission.goal_origin expected)
;;

let prepare_definition env root runtime =
  let native = Option.value_exn runtime.A.Runtime_builder.native_runtime in
  let capabilities =
    Lazy.force native.capabilities
    |> Result.map_error ~f:(fun error -> error.Chat_response.Tool_capability.message)
    |> Result.ok_or_failwith
  in
  let bundle =
    Chatmd_source_bundle.create
      ~root_file:"child.chatmd"
      ~sources:[ "child.chatmd", Birth.child_source ]
      ()
    |> Result.ok_or_failwith
  in
  A.Generated_definition.prepare
    ~env
    ~dir:Eio.Path.(Eio.Stdenv.fs env / root)
    ~revision_id:(P.Id.Prompt_revision.create ())
    ~created_at:(P.Timestamp.now ())
    ~current_capabilities:(fun () -> capabilities)
    ~references:[]
    bundle
  |> generated
;;

(* This is the same real backend policy after physical reopen. Any unexpected
   provider entry remains observable in the original fixture's counters. *)
let recovery_policy (counters : Retry_support.counters) =
  let base =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      Int.incr counters.providers;
      completed_response "historical negative unexpectedly reached provider")
  in
  { base with
    runtime_inference_ports =
      (fun actor ->
        Int.incr counters.inference_ports;
        base.runtime_inference_ports actor)
  }
;;

let reopen env sw root daemon policy ~after_close f =
  let store = Daemon.store daemon in
  Daemon.shutdown daemon |> protocol_ok;
  assert (S.Session_store.is_closed store);
  after_close ();
  let recovered =
    Daemon.start
      ~sw
      ~env:(env :> Eio_unix.Stdenv.base)
      ~config:(config root root (Filename.concat root "parent.chatmd"))
      ~tool_dir:root
      ~home:root
      ~process_start_identity:None
      ~options:
        { Daemon.default_options with
          inference_policy = policy
        ; qualify_chatml_extensions = true
        }
      ()
    |> protocol_ok
  in
  protect_cleanup
    ~finally:(fun () -> Daemon.shutdown recovered |> protocol_ok)
    (fun () -> f recovered)
;;

let historical_document original =
  let payload =
    match J.Document.payload original with
    | `Object fields ->
      `Object
        (List.filter_map fields ~f:(fun (name, value) ->
           if String.equal name "goal_descendants"
           then None
           else
             Some
               (name, if String.equal name "schema_version" then `Number "26" else value))
         @ [ "future_historical_origin", `String "retained" ])
    | _ -> failwith "actual child State is not an object"
  in
  let envelope =
    match J.Document.json original with
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (name, value) ->
           ( name
           , match name with
             | "version" -> `Number "11"
             | "payload" -> payload
             | _ -> value )))
    | _ -> failwith "actual child State envelope is not an object"
  in
  let changed = J.Document.inspect ~limits:J.Limits.default envelope |> document_ok in
  let retained_envelope document =
    match J.Document.json document with
    | `Object fields ->
      `Object
        (List.filter fields ~f:(fun (name, _) ->
           not (String.equal name "version" || String.equal name "payload")))
    | _ -> failwith "historical State envelope is not an object"
  in
  assert (Jsonaf.exactly_equal (retained_envelope original) (retained_envelope changed));
  assert (
    List.equal
      String.equal
      (J.Document.required_semantics original)
      (J.Document.required_semantics changed));
  changed
;;

let assert_snapshot_identity (before : S.Snapshot.t) (after : S.Snapshot.t) =
  [%test_eq: int] before.schema_version after.schema_version;
  assert (P.Id.Session.equal before.session_id after.session_id);
  [%test_eq: int64] before.transaction_sequence after.transaction_sequence;
  assert (Option.equal String.equal before.transaction_hash after.transaction_hash);
  [%test_eq: int64] before.event_sequence after.event_sequence;
  assert (P.Timestamp.equal before.created_at after.created_at);
  [%test_eq: string] before.prompt_artifact after.prompt_artifact;
  [%test_eq: string] before.workspace_identity after.workspace_identity
;;

(* Only a fully closed test image is replaced. Encode/validate through the same
   universal Snapshot carrier in a separate directory, then replace its exact
   original filename. No live Store/Actor endpoint can perform this conversion. *)
let omit_closed_component env ~root ~directory =
  let current_path = Eio.Path.(Eio.Stdenv.fs env / directory / "CURRENT") in
  let current_pointer = Eio.Path.load current_path in
  let before =
    S.Snapshot.load_current ~env ~directory ~max_payload_length:(64 * 1024 * 1024)
    |> Existing.store_ok
    |> Option.value_exn
  in
  let original =
    A.Session_persistence.restore_snapshot ~limits:J.Limits.default before.snapshot
    |> Existing.store_ok
    |> A.Session_persistence.Restored.state
  in
  assert (Option.is_some original.goal_descendants);
  let payload = historical_document before.snapshot.payload in
  let changed =
    S.Snapshot.with_value
      before.snapshot
      ~limits:J.Limits.default
      { (S.Snapshot.value before.snapshot) with payload }
    |> Existing.store_ok
  in
  let encoded_directory = Filename.concat root "historical-associated-image" in
  let encoded =
    S.Snapshot.install
      changed
      ~env
      ~directory:encoded_directory
      ~max_payload_length:(64 * 1024 * 1024)
    |> Existing.store_ok
  in
  [%test_eq: string] before.filename encoded.filename;
  let bytes =
    Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / encoded_directory / encoded.filename)
  in
  let decoded =
    S.Snapshot.decode_file ~max_payload_length:(64 * 1024 * 1024) bytes
    |> Existing.store_ok
  in
  assert_snapshot_identity before.snapshot decoded;
  assert (Jsonaf.exactly_equal (document payload) (document decoded.payload));
  let restored =
    A.Session_persistence.restore_snapshot ~limits:J.Limits.default decoded
    |> Existing.store_ok
  in
  A.Session_persistence.validate restored |> Existing.store_ok;
  assert (Option.is_none (A.Session_persistence.Restored.state restored).goal_descendants);
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    Eio.Path.(Eio.Stdenv.fs env / directory / before.filename)
    bytes;
  S.Durable_file.sync_directory ~env ~path:directory |> Existing.store_ok;
  [%test_eq: string] current_pointer (Eio.Path.load current_path);
  Historical.assert_saved env directory
;;

(* Execute recovery may already own the historical child's Session_start
   callback. Do not attempt a second start while that real moderator borrow is
   live; join its actual activation and checkpoint just as the stopped path does. *)
let prepare_recovered_child daemon (child : Registry.entry) =
  match (state child).lifecycle.observed with
  | Stopped -> Descendant_turn_adapter_tests.start_child daemon child
  | Idle
  | Starting
  | Recovering
  | Queued_for_slot
  | Running_turn _
  | Compacting _
  | Waiting_for_permission _
  | Stopping
  | Failed _ ->
    Factory.prepare_session_start (Daemon.factory daemon) child |> protocol_ok;
    Owner.ensure_loaded_authenticated child.runtime ~actor:original |> protocol_ok;
    let activation =
      Owner.with_loaded_runtime child.runtime (fun runtime ->
        Result.of_option
          runtime.A.Runtime_builder.moderator_activation
          ~error:
            (P.Error.invalid_request "real recovered child startup activation absent"))
      |> protocol_ok
    in
    let rec activation_finished () =
      if activation.pending ()
      then (
        Eio.Fiber.yield ();
        activation_finished ())
    in
    activation_finished ();
    let rec ready () =
      let current =
        Actor.with_moderator_checkpoint child.actor (fun () -> Actor.state child.actor)
        |> protocol_ok
      in
      match current.lifecycle.observed with
      | Failed error ->
        raise_s [%sexp "actual recovered child startup failed", (error : P.Error.t)]
      | Stopped -> failwith "actual recovered child stopped before Run admission"
      | Idle
        when Option.is_none current.active_operation
             && Option.is_some current.moderator
             && (not (List.is_empty current.moderator_executions))
             && List.for_all current.moderator_executions ~f:(fun receipt ->
               match receipt.P.Moderator_execution.status with
               | Completed _ -> true
               | Running | Failed _ | Interrupted _ -> false) -> ()
      | Idle
      | Starting
      | Recovering
      | Queued_for_slot
      | Running_turn _
      | Compacting _
      | Waiting_for_permission _
      | Stopping ->
        Eio.Fiber.yield ();
        ready ()
    in
    ready ()
;;

let assert_run_rejected
      env
      daemon
      parent
      child
      (counters : Retry_support.counters)
      ~key
      ~expected_message
  =
  (* Genuine compilation, original authorization, source ownership and Init are
     retained; rejection is not manufactured by a missing observer or unloaded
     runtime. The source itself asks for no inference during initialization. *)
  let before_start = state child in
  let providers_before_start = !(counters.providers) in
  assert (Option.is_none before_start.goal_descendants);
  prepare_recovered_child daemon child;
  let before = state child in
  [%test_eq: int] providers_before_start !(counters.providers);
  assert (Jsonaf.exactly_equal (ledger before_start) (ledger before));
  let parent_before = state parent in
  let provider_count = !(counters.providers) in
  assert (Option.is_none before.goal_descendants);
  assert (Option.is_none before.active_operation);
  let request =
    A.Run_admission_request.Headless.create
      ~session_id:before.identity.session_id
      ~generation:before.identity.generation
      ~expected_revision:before.counters.revision
      ~mode:Single_turn
      ~input:
        (User_submission
           { kind = Plain_text
           ; text = "A real ordinary Run must preserve unknown inherited governance."
           ; attachments = []
           })
      ~key:(P.Idempotency_key.of_string key |> protocol_ok)
    |> protocol_ok
  in
  let outcome =
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 20. (fun () ->
      Goal_service_tests.ordinary daemon child request)
  in
  (match outcome with
   | A.Run_admission_outcome.Rejected error ->
     [%test_eq: P.Error.code] Permission_denied error.code;
     (* Pin the rejection boundary: an earlier authorization failure must not
        satisfy this origin-classification test. This is an output assertion,
        never production error dispatch or a retry predicate. *)
     [%test_eq: string] expected_message error.message
   | Admitted _ -> failwith "historical missing accounting admitted a real Run"
   | Uncertain error -> raise_s [%sexp "unexpected uncertain Run", (error : P.Error.t)]);
  let after = state child in
  assert (Option.is_none after.goal_descendants);
  assert (Option.is_none after.active_operation);
  assert (Jsonaf.exactly_equal (ledger before) (ledger after));
  assert (Jsonaf.exactly_equal (runs before) (runs after));
  assert (
    Option.equal
      Jsonaf.exactly_equal
      (accounting parent_before)
      (accounting (state parent)));
  [%test_eq: int] provider_count !(counters.providers)
;;

let assert_retained daemon reference admission origin =
  let actual = resolve daemon reference in
  assert (D.Reference.equal reference (D.reference actual));
  assert (D.equal_stage actual.stage Linked);
  assert (Option.is_none actual.revocation);
  assert_origin actual origin;
  [%test_eq: string] admission (Historical.admission actual)
;;

let%expect_test
    "historical private Unknown stays unknown through actual Factory Run denial"
  =
  with_recoverable_fixture
    ~timeout:120.
    (fun env sw root daemon policy parent runtime _revision counters ->
       let definition = prepare_definition env root runtime in
       (* Actual management creation supplies the original source/principal/
          inference capture. The sibling below is a separate first reservation;
          the seed's immutable positive Unrelated admission is never changed. *)
       let seed =
         Factory.create_generated_session
           ~start_immediately:false
           (Daemon.factory daemon)
           ~parent_session_id:(state parent).identity.session_id
           ~idempotency_key:
             (P.Idempotency_key.of_string "554-historical-seed" |> protocol_ok)
           ~display_name:None
           definition
         |> protocol_ok
       in
       let seed_reference = Option.value_exn (state seed).spec.delegation in
       let seed_record = resolve daemon seed_reference in
       assert_origin seed_record (Some Origin.unrelated);
       let seed_admission = Historical.admission seed_record in
       let current = state parent in
       let key : D.Key.t =
         { seed_record.key with
           idempotency_key =
             P.Idempotency_key.of_string "554-historical-unknown" |> protocol_ok
         }
       in
       let admission : D.Admission.t =
         { seed_record.admission with
           child_session_id = P.Id.Session.create ()
         ; transaction_id = P.Id.Transaction.create ()
         ; goal_origin = None
         }
       in
       let store = Daemon.store daemon in
       let delegations = S.Session_store.delegations store in
       let moderator = A.Runtime_builder.moderation_source runtime |> protocol_ok in
       let check_parent (actual : A.Session_state.t) =
         let open Result.Let_syntax in
         let%bind fingerprint = A.Delegation_authority.fingerprint ?moderator actual in
         assert (P.Id.Session.equal actual.identity.session_id key.parent_session_id);
         assert (Int.equal actual.identity.generation key.parent_generation);
         assert (
           P.Id.Principal.equal
             key.principal_id
             (Operator_authorization.principal original).id);
         assert (Int.equal actual.identity.generation current.identity.generation);
         assert (Int64.equal actual.stop_epoch current.stop_epoch);
         assert (P.Session.equal_desired_state actual.lifecycle.desired Running);
         assert (not actual.halted);
         assert (Option.is_none actual.failure);
         [%test_eq: string] admission.authority_sha256 fingerprint;
         Ok ()
       in
       Actor.checkpoint parent.actor ~persist:check_parent |> protocol_ok;
       let record =
         D.reserve
           delegations
           ~key
           ~request_sha256:seed_record.request_sha256
           ~admission
           ~max_records:64
           ~max_bytes:(64 * 1024 * 1024)
         |> Existing.store_ok
         |> function
         | D.New record -> record
         | Replay _ | Conflict _ -> failwith "Unknown sibling was not a first reservation"
       in
       assert_origin record None;
       let artifacts =
         S.Prompt_artifact_store.create
           ~env
           ~root:(S.Data_root.prompt_artifacts_path (S.Session_store.data_root store))
         |> Existing.store_ok
       in
       let record =
         A.Generated_definition.install_reserved
           ~delegations
           ~reservation:record
           ~artifact_store:artifacts
           definition
         |> generated
       in
       let handle = Historical.install env sw store current definition record in
       let directory = S.Session_store.Handle.snapshot_directory handle in
       let reference = D.reference record in
       let admission = Historical.admission record in
       let record = D.advance delegations record Child_installed |> Existing.store_ok in
       Actor.checkpoint parent.actor ~persist:check_parent |> protocol_ok;
       D.advance delegations record Linked |> Existing.store_ok |> ignore;
       S.Session_store.close_session store handle |> Existing.store_ok;
       Historical.assert_saved env directory;
       reopen env sw root daemon policy ~after_close:ignore (fun recovered ->
         let parent =
           Registry.load (Daemon.registry recovered) current.identity.session_id
           |> protocol_ok
         in
         let child =
           Registry.load (Daemon.registry recovered) reference.child_session_id
           |> protocol_ok
         in
         assert_retained recovered reference admission None;
         assert_run_rejected
           env
           recovered
           parent
           child
           counters
           ~key:"554-unknown-real-run"
           ~expected_message:
             "child origin inherited Goal ancestry is historically unknown";
         assert_retained recovered seed_reference seed_admission (Some Origin.unrelated);
         reopen env sw root recovered policy ~after_close:ignore (fun reopened ->
           let child =
             Registry.load (Daemon.registry reopened) reference.child_session_id
             |> protocol_ok
           in
           assert (Option.is_none (state child).goal_descendants);
           assert_retained reopened reference admission None;
           Historical.assert_saved env directory)));
  print_endline
    "actual private Unknown: load, Run denied, no canonical/provider, reopen unchanged";
  [%expect
    {| actual private Unknown: load, Run denied, no canonical/provider, reopen unchanged |}]
;;

let%expect_test "actual Associated origin cannot recover missing accounting as Unrelated" =
  Birth.with_native_goal_child
    ~timeout:120.
    (fun env sw daemon parent child _scope reference counters ->
       let store = Daemon.store daemon in
       let record = resolve daemon reference in
       let origin = Option.value_exn record.admission.goal_origin in
       assert (not (List.is_empty (Origin.scopes origin)));
       let admission = Historical.admission record in
       let parent_id = (state parent).identity.session_id in
       let root = Filename.dirname (S.Data_root.path (S.Session_store.data_root store)) in
       let directory =
         S.Session_store.Handle.snapshot_directory (Option.value_exn child.store_handle)
       in
       let policy = recovery_policy counters in
       reopen
         env
         sw
         root
         daemon
         policy
         ~after_close:(fun () -> omit_closed_component env ~root ~directory)
         (fun recovered ->
            let parent =
              Registry.load (Daemon.registry recovered) parent_id |> protocol_ok
            in
            let child =
              Registry.load (Daemon.registry recovered) reference.child_session_id
              |> protocol_ok
            in
            assert_retained recovered reference admission (Some origin);
            assert_run_rejected
              env
              recovered
              parent
              child
              counters
              ~key:"554-associated-real-run"
              ~expected_message:"inherited child origin has no scoped accounting";
            assert_retained recovered reference admission (Some origin);
            reopen env sw root recovered policy ~after_close:ignore (fun reopened ->
              let child =
                Registry.load (Daemon.registry reopened) reference.child_session_id
                |> protocol_ok
              in
              assert (Option.is_none (state child).goal_descendants);
              assert_retained reopened reference admission (Some origin);
              Historical.assert_saved env directory)));
  print_endline
    "actual Associated: omitted component denied, no canonical/provider, exact origin \
     retained";
  [%expect
    {| actual Associated: omitted component denied, no canonical/provider, exact origin retained |}]
;;
