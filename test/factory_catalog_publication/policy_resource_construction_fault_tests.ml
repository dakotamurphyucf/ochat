open! Core
open Agent_server_test_support
open Retry_support
module Runtime = Goal_resume_runtime_tests
module Fault = Inference_close_tests.Fault
module Ledger = A.Inference_ledger
module Observation = Inference.Observation
module Store = Agent_store

let assert_pending_cleanup entry =
  let invoked = ref false in
  let maintenance =
    Owner.with_unloaded entry.Registry.runtime (fun () ->
      invoked := true;
      Ok ())
    |> protocol_ok
  in
  assert (Option.is_none maintenance);
  assert (not !invoked);
  assert (not (Owner.reserve_inactive_close entry.runtime))
;;

let%expect_test
    "failed Factory construction retains its physical partial and retries only graph \
     facts"
  =
  let fault = Fault.create () in
  let constructor_providers = ref 0 in
  let entry_disposal = ref None in
  let daemon_disposal = ref None in
  Runtime.with_cold_episode
    ~observe_entry_close:(fun report ->
      assert (Option.is_none !entry_disposal);
      entry_disposal := Some report)
    ~observe_shutdown:(fun report ->
      assert (Option.is_none !daemon_disposal);
      daemon_disposal := Some report)
    ~wrap_env:(Fault.wrap_env fault)
    ~post_stream:(fun ~sw:_ ~inputs ->
      let transcript =
        `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string
      in
      if String.is_substring transcript ~substring:Runtime.constructor_input
      then (
        Int.incr constructor_providers;
        (* Actual native Running ACK already precedes this backend boundary.
           Fail only later projection preflight, never the canonical writer. *)
        fault.phase <- Reject_settlement);
      completed_response "591 actual failed construction provider")
    Legacy
    Continue
    (fun _env
      daemon
      entry
      _old_runtime
      counters
      _probe
      session
      _admission
      _initial
      stopped
      actor
      request ->
       let store = Daemon.store daemon in
       let handle = Option.value_exn entry.Registry.store_handle in
       fault.marker
       <- Some
            (Filename.concat
               (Store.Data_root.indexes_path (Store.Session_store.data_root store))
               "sessions.recovery-required");
       fault.cache
       <- Some
            (Filename.concat
               (Store.Session_store.Handle.cache_directory handle)
               "cache.bin");
       let disarm () = fault.phase <- Quiet in
       protect_cleanup ~finally:disarm (fun () ->
         let before = Inference_close_tests.marks stopped in
         let primary =
           match
             Runtime.Service.resume_episode
               (Runtime.public daemon)
               ~entry
               ~actor
               ~session
               ~request
             |> protocol_ok
           with
           | Resume_failed { error; _ } -> error
           | Rejected error | Uncertain error ->
             raise_s
               [%sexp
                 "constructor did not reach its actual command ACK", (error : P.Error.t)]
           | Resumed _ | Retained _ -> failwith "failed constructor published a runtime"
         in
         [%test_eq: P.Error.code] Invalid_state primary.code;
         [%test_eq: bool] false primary.retryable;
         assert (
           String.is_substring primary.message ~substring:"566 prewriter projection read");
         assert (fault.settlement_rejections > 0);
         [%test_eq: int] 1 !constructor_providers;
         [%test_eq: int] 2 !(counters.providers);
         assert (not (Owner.is_loaded entry.runtime));
         (* Runtime.start_moderator entered only after Build.physical retained
            the full actual Factory closer. Its failed initialization close saved
            cache once but could not finish this SAME registered main graph. *)
         [%test_eq: int] 1 fault.cache_renames;
         let failed = state entry in
         let running =
           List.filter (Ledger.rows failed.inference_ledger) ~f:(fun row ->
             match Observation.Attempt_record.state (Ledger.Row.record row) with
             | Running -> true
             | Prepared | Terminal _ | Interrupted _ -> false)
           |> Runtime.one_actual "actual unfinished constructor native registration"
         in
         let native = Ledger.Row.handle running in
         let admitted = Inference_close_tests.marks failed in
         [%test_eq: int64] (Int64.succ before.attempt) admitted.attempt;
         [%test_eq: int64] before.turn admitted.turn;
         assert_pending_cleanup entry;
         let same_error expected = function
           | Error actual ->
             assert (
               Jsonaf.exactly_equal (P.Error.to_json expected) (P.Error.to_json actual))
           | Ok () -> failwith "pending constructor cleanup was reported joined"
         in
         same_error
           (P.Error.create
              Conflict
              ~message:"runtime cleanup must finish before fresh admission"
              ~retryable:true
              ())
           (Owner.unload entry.runtime);
         assert (not (Owner.is_loaded entry.runtime));
         (* This Model Job must settle before its callback and graph. A finite
            detach cannot retry it; the joined path reaches the genuine writer. *)
         let expected =
           let marker = Option.value_exn fault.marker in
           Store.Store_error.of_exn
             ~operation:"read projection recovery marker"
             ~path:marker
             (Core_unix.Unix_error (EIO, "566 prewriter projection read", marker))
           |> fun error ->
           P.Error.create
             Persistence_error
             ~message:(Sexp.to_string_hum (Store.Store_error.sexp_of_t error))
             ~retryable:false
             ()
         in
         same_error expected (Owner.unload_and_wait entry.runtime);
         assert_pending_cleanup entry;
         assert (not (Owner.is_loaded entry.runtime));
         [%test_eq: int] 1 fault.cache_renames;
         let retained =
           Ledger.find
             (state entry).inference_ledger
             ~ordinal:(Ledger.Handle.ordinal native)
           |> Option.value_exn
         in
         assert (Ledger.Handle.equal native (Ledger.Row.handle retained));
         (match Observation.Attempt_record.state (Ledger.Row.record retained) with
          | Running -> ()
          | Prepared | Terminal _ | Interrupted _ ->
            failwith "failed cleanup changed the actual acknowledged native row");
         (* Keep this fixture's finite-unload and subsequent maintenance path.
            The public-initializer fixture below exercises permanent Retained
            disposal followed by a successful retry on the same actual owner. *)
         [%test_eq: int] 1 fault.cache_renames;
         [%test_eq: int] 1 !constructor_providers;
         (* Explicit joined retirement settles Job, callback and factual graph
            closure on the SAME live Actor/writer without replaying construction. *)
         disarm ();
         Owner.unload_and_wait entry.runtime |> protocol_ok;
         Owner.unload_and_wait entry.runtime |> protocol_ok;
         [%test_eq: int] 1 fault.cache_renames;
         [%test_eq: int] 1 !constructor_providers;
         [%test_eq: int] 2 !(counters.providers);
         let final = state entry in
         assert (
           A.Canonical_usage_fold.Marks.equal admitted (Inference_close_tests.marks final));
         let ended =
           Ledger.find final.inference_ledger ~ordinal:(Ledger.Handle.ordinal native)
           |> Option.value_exn
         in
         assert (Ledger.Handle.equal native (Ledger.Row.handle ended));
         (match Observation.Attempt_record.state (Ledger.Row.record ended) with
          | Interrupted { reason = Host_interrupted; delivery = Response_started } -> ()
          | Prepared | Running | Terminal _ | Interrupted _ ->
            raise_s
              [%sexp
                "constructor factual cleanup changed native outcome"
              , (Observation.Attempt_record.to_json (Ledger.Row.record ended) : Jsonaf.t)]);
         let maintenance = ref false in
         (match
            Owner.with_unloaded entry.runtime (fun () ->
              maintenance := true;
              Ok ())
            |> protocol_ok
          with
          | Some () -> ()
          | None -> failwith "positive same-owner graph closure left cleanup pending");
         assert !maintenance;
         assert (not (Owner.is_loaded entry.runtime));
         let disposed = Owner.close_and_wait_reported entry.runtime in
         assert (
           Owner.Disposal_report.equal_disposition
             Joined
             (Owner.Disposal_report.disposition disposed));
         assert (not (List.is_empty (Owner.Disposal_report.failures disposed)));
         assert (phys_equal disposed (Owner.close_and_wait_reported entry.runtime));
         [%test_eq: int] 1 fault.cache_renames;
         [%test_eq: int] 1 !constructor_providers;
         [%test_eq: int] 2 !(counters.providers)));
  let entry_report = Option.value_exn !entry_disposal in
  let daemon_report = Option.value_exn !daemon_disposal in
  let entry_failures = Agent_server.Session_disposal_report.failures entry_report in
  let daemon_failures = Agent_server.Session_disposal_report.failures daemon_report in
  assert (not (List.is_empty entry_failures));
  List.iter entry_failures ~f:(fun actual ->
    assert (List.mem daemon_failures actual ~equal:phys_equal));
  Fault.assert_settlement_diagnostic fault [%expect.output];
  print_endline
    "original constructor failure retained; maintenance excluded; physical close once \
     and same graph facts retried";
  [%expect
    {| original constructor failure retained; maintenance excluded; physical close once and same graph facts retried |}]
;;

(* A failed public initializer still owns its actual prepublication Entry. The
   outer Factory error continuation must not release that Entry's actor lock
   while its real graph/resource cleanup remains retained by the Registry. *)
let%expect_test
    "failed public initialization retains its actual handle until shutdown retry"
  =
  let fault = Fault.create () in
  let providers = ref 0 in
  let actual_actor = ref None in
  let data_root = ref None in
  let admitted = ref None in
  let registry_disposal = ref None in
  let daemon_disposal = ref None in
  let recipe = "627-init-recipe.chatmd" in
  let marker = "627_ACTUAL_INIT_MODEL" in
  let prompt_text =
    {|<developer>Retained public initialization ownership.</developer>
<tool name="init_recipe" agent="627-init-recipe.chatmd" local/>
<script id="initializer" language="chatml" kind="moderator">
let initial_state = 0
let on_event ctx state event = match event with
| `Session_start ->
  let* called = Model.call("agent_prompt_v1", `Object([
    {key = "prompt"; value = `String("627-init-recipe.chatmd")},
    {key = "input"; value = `String("627_ACTUAL_INIT_MODEL")},
    {key = "is_local"; value = `Bool(true)}
  ])) in
  (match called with
  | `Ok(value) -> Task.pure(state + 1)
  | `Refused(message) -> Task.fail(message)
  | `Error(message) -> Task.fail(message))
| _ -> Task.pure(state)
</script>|}
  in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs ->
      let transcript =
        `Array (List.map inputs ~f:Res.Item.jsonaf_of_t) |> Jsonaf.to_string
      in
      assert (String.is_substring transcript ~substring:marker);
      Int.incr providers;
      [%test_eq: int] 1 !providers;
      let actor = Option.value_exn !actual_actor in
      let before = Actor.state actor |> protocol_ok in
      fault.cache
      <- Some
           (Filename.concat
              (Store.Data_root.session_path
                 (Option.value_exn !data_root)
                 before.identity.session_id)
              "cache/cache.bin");
      assert (Option.is_none before.policy_canonical_usage);
      let row =
        Runtime.one_actual
          "actual fresh initializer native row"
          (Ledger.rows before.inference_ledger)
      in
      (match Observation.Attempt_record.state (Ledger.Row.record row) with
       | Running -> ()
       | Prepared | Terminal _ | Interrupted _ ->
         failwith "public initializer provider preceded its actual Running ACK");
      admitted := Some (row, Inference_close_tests.marks before);
      (* Only later projection reads fail. The canonical writer is never made
         uncertain, so the SAME retained writer can positively retry cleanup. *)
      fault.phase <- Reject_settlement;
      completed_response "627 actual initializer response")
  in
  let policy =
    { policy with
      runtime_inference_ports =
        (fun actor ->
          (match !actual_actor with
           | None -> actual_actor := Some actor
           | Some original -> assert (phys_equal actor original));
          policy.runtime_inference_ports actor)
    }
  in
  Creation_tests.with_daemon
    ~observe_shutdown:(fun report ->
      assert (Option.is_none !daemon_disposal);
      daemon_disposal := Some report)
    ~prompt_text
    ~sources:[ recipe, "<developer>627 actual initializer recipe.</developer>" ]
    ~wrap_env:(Fault.wrap_env fault)
    ~policy
    (fun env _sw _root daemon ->
       let registry = Daemon.registry daemon in
       let store = Daemon.store daemon in
       data_root := Some (Store.Session_store.data_root store);
       fault.marker
       <- Some
            (Filename.concat
               (Store.Data_root.indexes_path (Store.Session_store.data_root store))
               "sessions.recovery-required");
       let client = connection daemon principal in
       initialize client;
       let finish () =
         fault.phase <- Quiet;
         Agent_client.Connection.close client;
         let report = Registry.shutdown_reported registry in
         assert (Agent_server.Session_disposal_report.disposed report);
         match !registry_disposal with
         | None -> registry_disposal := Some report
         | Some original -> assert (phys_equal original report)
       in
       let body () =
         Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
           let primary =
             match
               Agent_client.Connection.request
                 client
                 (Session_create (create_request ~key:"627-failed-public-init" ()))
             with
             | Error failure -> failure
             | Ok _ ->
               failwith "public initializer ignored its actual selected IO failure"
           in
           [%test_eq: P.Error.code] Invalid_state primary.code;
           [%test_eq: bool] false primary.retryable;
           assert (
             String.is_substring
               primary.message
               ~substring:"566 prewriter projection read");
           assert (fault.settlement_rejections > 0);
           [%test_eq: int] 1 fault.cache_renames;
           [%test_eq: int] 1 !providers;
           let actor = Option.value_exn !actual_actor in
           let retained = Actor.state actor |> protocol_ok in
           let session_id = retained.identity.session_id in
           assert (Registry.has_pending_cleanup registry session_id);
           assert (Option.is_none (Registry.find registry session_id));
           let row, marks = Option.value_exn !admitted in
           let native = Ledger.Row.handle row in
           let pending =
             Runtime.one_actual
               "same retained initializer native row"
               (Ledger.rows retained.inference_ledger)
           in
           assert (Ledger.Handle.equal native (Ledger.Row.handle pending));
           assert (
             A.Canonical_usage_fold.Marks.equal
               marks
               (Inference_close_tests.marks retained));
           let job =
             Runtime.one_actual "actual fresh initializer Model Job" retained.jobs
           in
           [%test_eq: P.Job.kind] Model_call job.kind;
           [%test_eq: int] 1 job.attempt;
           assert (
             Option.equal P.Inference_origin.equal (Some Unbound) job.inference_origin);
           assert (Option.is_none job.launch);
           (match Registry.load registry session_id with
            | Error failure -> [%test_eq: P.Error.code] Conflict failure.code
            | Ok _ -> failwith "failed initializer admitted a second same-ID owner");
           Registry.with_lifecycle registry (P.Id.Session.create ()) (fun _ -> Ok ())
           |> protocol_ok;
           let retained_disposal = Registry.shutdown_reported registry in
           assert (not (Agent_server.Session_disposal_report.disposed retained_disposal));
           assert (Registry.has_pending_cleanup registry session_id);
           [%test_eq: int] 1 fault.cache_renames;
           [%test_eq: int] 1 !providers;
           (* Registry exclusion alone cannot prove Store custody. Acquiring this
             SAME durable ID must still encounter the original actual actor lock.
             An accidental RED success is closed before reporting the failure. *)
           Eio.Switch.run (fun sw ->
             match
               Store.Session_store.open_session
                 store
                 ~sw
                 ~actor_lock_nonce:
                   (P.Id.Transaction.create () |> P.Id.Transaction.to_string)
                 session_id
             with
             | Error (Locked _) -> ()
             | Error failure ->
               raise_s
                 [%sexp
                   "unexpected retained lock rejection", (failure : Store.Store_error.t)]
             | Ok handle ->
               Store.Session_store.close_session store handle |> Existing.store_ok;
               failwith
                 "outer public failure released the retained initializer actor lock");
           fault.phase <- Quiet;
           let disposed = Registry.shutdown_reported registry in
           assert (Agent_server.Session_disposal_report.disposed disposed);
           assert (
             not (List.is_empty (Agent_server.Session_disposal_report.failures disposed)));
           registry_disposal := Some disposed;
           assert (phys_equal disposed (Registry.shutdown_reported registry));
           assert (not (Registry.has_pending_cleanup registry session_id));
           assert (Option.is_none (Registry.find registry session_id));
           assert (Result.is_error (Actor.state actor));
           [%test_eq: int] 1 fault.cache_renames;
           [%test_eq: int] 1 !providers;
           Eio.Switch.run (fun sw ->
             let handle =
               Store.Session_store.open_session
                 store
                 ~sw
                 ~actor_lock_nonce:
                   (P.Id.Transaction.create () |> P.Id.Transaction.to_string)
                 session_id
               |> Existing.store_ok
             in
             Exn.protect
               ~finally:(fun () ->
                 Store.Session_store.close_session store handle |> Existing.store_ok)
               ~f:(fun () ->
                 let saved =
                   Factory.read_owned_session (Daemon.factory daemon) handle
                   |> protocol_ok
                 in
                 [%test_eq: P.Id.Session.t] session_id saved.identity.session_id;
                 assert (
                   A.Canonical_usage_fold.Marks.equal
                     marks
                     (Inference_close_tests.marks saved));
                 let saved_row =
                   Runtime.one_actual
                     "same persisted initializer native row"
                     (Ledger.rows saved.inference_ledger)
                 in
                 assert (Ledger.Handle.equal native (Ledger.Row.handle saved_row));
                 (match
                    Observation.Attempt_record.state (Ledger.Row.record saved_row)
                  with
                  | Terminal _ | Interrupted _ -> ()
                  | Prepared | Running ->
                    failwith "positive cleanup left native work open");
                 let saved_job =
                   Runtime.one_actual "same persisted initializer Model Job" saved.jobs
                 in
                 [%test_eq: P.Id.Job.t] job.id saved_job.id;
                 [%test_eq: int] 1 saved_job.attempt;
                 assert (Option.is_some (P.Job.terminal_result saved_job |> protocol_ok));
                 [%test_eq: int] 1 !providers)))
       in
       (* No provider or test gate remains live. Disarm before the genuine retry;
         preserve a failing assertion's rawtrace even if the unpatched owner can
         now report only a closed-Handle cleanup failure. *)
       match body () with
       | () -> Eio.Cancel.protect finish
       | exception primary ->
         let backtrace = Stdlib.Printexc.get_raw_backtrace () in
         let cleanup =
           try
             Eio.Cancel.protect finish;
             None
           with
           | failure -> Some (failure, Stdlib.Printexc.get_raw_backtrace ())
         in
         Option.iter cleanup ~f:(fun (failure, cleanup_backtrace) ->
           try
             Eio.traceln
               "627 secondary cleanup failure: %s\n%s"
               (Exn.to_string failure)
               (Stdlib.Printexc.raw_backtrace_to_string cleanup_backtrace)
           with
           | _ -> ());
         Exn.raise_with_original_backtrace primary backtrace);
  let registry_report = Option.value_exn !registry_disposal in
  let daemon_report = Option.value_exn !daemon_disposal in
  let failures = Agent_server.Session_disposal_report.failures registry_report in
  let forwarded = Agent_server.Session_disposal_report.failures daemon_report in
  assert (not (List.is_empty failures));
  List.iter failures ~f:(fun actual ->
    assert (List.mem forwarded actual ~equal:phys_equal));
  Fault.assert_settlement_diagnostic fault [%expect.output];
  print_endline
    "actual failed public initializer retains actor lock; same-owner shutdown retries \
     without provider replay";
  [%expect
    {| actual failed public initializer retains actor lock; same-owner shutdown retries without provider replay |}]
;;
