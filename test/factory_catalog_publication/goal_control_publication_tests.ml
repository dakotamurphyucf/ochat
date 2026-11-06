open! Core
open Agent_server_test_support
open Retry_support
module Goal = Goal_service_tests
module Native = Goal_control_completion_tests
module Service = Agent_server.Goal_run_service
module Persistence = A.Session_persistence
module Store = Agent_store

(* One known-ACK publication cut, separate from
   goal_control_boundary_tests' intentionally UNKNOWN writer cut.

   Legacy Schedule.after_ms independently writes/installs/publishes its producer
   BEFORE returning to this callback. Capture that real earlier transaction at
   After_sync; the later controlling parent ACK joins control and Completed
   callback, retaining the already-acknowledged producer. No atomic producer +
   control checkpoint is invented.

   The actual Actor commit_queued_event sequence is:
     writer returns -> install_committed_transition (t.state <- candidate)
     -> install_callback_goal_control -> committed/completed bookkeeping
     -> install_native_goal_plan -> publish_committed_transition
     -> sync_extension_clock -> services.monotonic_now.
   Intermediate Store/Actor installation code does not sample the mono clock.
   The one-shot is armed by exact parent After_sync and throws only at the next
   actual clock sample. Frozen background polling clock keeps other consumers
   asleep; no alternate publish callback or production testing API is introduced.

   Unexpected Actor exceptions close its mailbox and fail the daemon switch.
   Therefore the original exception is observed inside the protected PUBLIC call,
   and retained state is then independently read through real physical Recovery,
   after the failed switch has joined. Fresh Store/session owners must acquire
   both physical locks; this does not claim graceful shutdown of the dead Actor.
   An exact post-fault cache rename witnesses partial runtime.close after its
   actual inference-session releases. A closed Actor is never queried. *)
exception Publication_failed of unit ref

(* Legacy Schedule.after_ms persists the complete ChatML value snapshot. The
   script's `String(payload) is a Lang variant, not a raw Jsonaf string. *)
let producer_payload =
  Chatml.Chatml_lang.(
    VVariant ("String", [ VString "actual known-ACK publication producer" ]))
  |> Chatml.Chatml_value_codec.Snapshot.of_value_exn
  |> Chatml.Chatml_value_codec.Snapshot.to_jsonaf
;;

module Selection = struct
  type t =
    { transaction : Store.Transaction.t
    ; producer_transaction : Store.Transaction.t
    ; receipt : A.Goal_command_receipt.t
    ; callback : P.Moderator_execution.t
    ; schedule : P.Schedule.t
    }
end

module Fault = struct
  type t =
    { primary : exn
    ; on_publication : (unit -> unit) option
    ; mutable session : P.Id.Session.t option
    ; mutable selected : Selection.t option
    ; mutable producer : (Store.Transaction.t * P.Schedule.t) option
    ; mutable armed : bool
    ; mutable ack_hits : int
    ; mutable publication_hits : int
    ; mutable cache_file : string option
    ; mutable partial_close_hits : int
    ; mutable pause_clocks : unit -> unit
    ; mutable resume_clocks : unit -> unit
    }

  let create ?on_publication () =
    { primary = Publication_failed (ref ())
    ; on_publication
    ; session = None
    ; selected = None
    ; producer = None
    ; armed = false
    ; ack_hits = 0
    ; publication_hits = 0
    ; cache_file = None
    ; partial_close_hits = 0
    ; pause_clocks = ignore
    ; resume_clocks = ignore
    }
  ;;

  let reached t env path =
    match t.session with
    | None -> ()
    | Some session ->
      let scan =
        Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
        |> Store.Journal_segment.scan_contents ~max_payload_length:(64 * 1024 * 1024)
        |> Existing.store_ok
      in
      (match List.last scan.entries with
       | Some frame when Int.equal (Store.Frame.flags frame.frame) 0 ->
         let transaction =
           Store.Transaction.decode (Store.Frame.payload frame.frame) |> Existing.store_ok
         in
         if P.Id.Session.equal session transaction.session_id
         then (
           let atoms =
             A.Session_delta_document.decode ~limits:D.Limits.default transaction.delta
             |> document_ok
             |> A.Session_delta_document.value
             |> Goal_admission_fault_tests.atoms
           in
           let producers =
             List.filter_map atoms ~f:(function
               | A.Session_delta.Schedule_changed schedule
                 when Jsonaf.exactly_equal schedule.payload producer_payload ->
                 Some schedule
               | _ -> None)
           in
           if not (List.is_empty producers)
           then (
             let schedule =
               Goal_resume_runtime_tests.one_actual
                 "actual independent producer transaction"
                 producers
             in
             assert (Option.is_none t.producer);
             t.producer <- Some (transaction, schedule));
           let receipt =
             List.find_map atoms ~f:(function
               | A.Session_delta.Goal_catalog_changed catalog ->
                 List.find (A.Goal_catalog.receipts catalog) ~f:(fun receipt ->
                   String.equal
                     Native.control_key
                     (P.Idempotency_key.to_string receipt.A.Goal_command_receipt.key))
               | _ -> None)
           in
           match receipt with
           | None -> ()
           | Some receipt ->
             (* The first controlling catalog frame MUST be its actual joined
                parent checkpoint, not merely a native Complete or other write. *)
             let callback =
               List.filter_map atoms ~f:(function
                 | Moderator_execution_changed callback ->
                   (match
                      callback.P.Moderator_execution.context.phase, callback.status
                    with
                    | Session_resume, Completed _ -> Some callback
                    | _ -> None)
                 | _ -> None)
               |> Goal_resume_runtime_tests.one_actual
                    "known controlling parent checkpoint"
             in
             let producer_transaction, schedule = Option.value_exn t.producer in
             assert (
               P.Id.Session.equal transaction.session_id producer_transaction.session_id);
             [%test_eq: int] transaction.generation producer_transaction.generation;
             assert (
               Int64.compare
                 producer_transaction.transaction_sequence
                 transaction.transaction_sequence
               < 0);
             assert (
               Int64.compare
                 producer_transaction.session_revision
                 transaction.session_revision
               < 0);
             t.session <- None;
             t.selected
             <- Some
                  { Selection.transaction
                  ; producer_transaction
                  ; receipt
                  ; callback
                  ; schedule
                  };
             t.ack_hits <- t.ack_hits + 1;
             t.armed <- true)
       | Some _ | None -> ())
  ;;

  let wrap t env =
    let real_mono, pause_mono, resume_mono, _ =
      controlled_monotonic_clock env#mono_clock
    in
    let clock, pause_wall, resume_wall, _ =
      controlled_wall_clock env#clock ~initial:(Eio.Time.now env#clock)
    in
    t.pause_clocks
    <- (fun () ->
         pause_wall ();
         pause_mono ());
    t.resume_clocks
    <- (fun () ->
         resume_wall ();
         resume_mono ());
    let module Clock = struct
      type t = unit
      type time = Mtime.t

      let now () =
        if t.armed
        then (
          t.armed <- false;
          t.publication_hits <- t.publication_hits + 1;
          match t.on_publication with
          | None -> raise t.primary
          | Some on_publication -> on_publication ());
        Eio.Time.Mono.now real_mono
      ;;

      (* No test deadline uses this injected clock. Existing producer sleeps
         retain their ordinary Eio path and do not consume the throwing sample. *)
      let sleep_until () deadline = Eio.Time.Mono.sleep_until real_mono deadline
    end
    in
    let mono_clock = Eio.Resource.T ((), Eio.Time.Pi.clock (module Clock)) in
    let clocked =
      object
        method clock = clock
        method mono_clock = mono_clock
        method domain_mgr = env#domain_mgr
        method stdin = env#stdin
        method stdout = env#stdout
        method stderr = env#stderr
        method net = env#net
        method process_mgr = env#process_mgr
        method fs = env#fs
        method cwd = env#cwd
        method secure_random = env#secure_random
        method debug = env#debug
        method backend_id = env#backend_id
      end
    in
    let observed =
      Crash_fault_io.wrap
        clocked
        ~boundary:After_rename
        ~matches:(fun path ->
          Option.exists t.cache_file ~f:(fun cache_file ->
            String.equal (Filename.dirname path) (Filename.dirname cache_file)
            && String.is_prefix (Filename.basename path) ~prefix:".cache.bin."
            && String.is_suffix path ~suffix:".tmp"))
        ~reached:(fun path ->
          if Int.equal t.publication_hits 1
          then (
            [%test_eq: string] (Option.value_exn t.cache_file) path;
            t.partial_close_hits <- t.partial_close_hits + 1))
    in
    Crash_fault_io.wrap
      observed
      ~boundary:After_sync
      ~matches:(fun path ->
        String.is_substring path ~substring:"/journal/"
        && String.is_suffix path ~suffix:".log")
      ~reached:(reached t env)
  ;;
end

let prompt =
  Native.prompt Pending_cold Hold_progress
  |> Native.replace_prompt
       ~pattern:{|let* held = call("goal_hold", control(before, "hold")) in|}
       ~with_:
         {|let* producer = Schedule.after_ms(600000, `String("actual known-ACK publication producer")) in
    let* held = call("goal_hold", control(before, "hold")) in|}
;;

module Recovery_paths = struct
  type t =
    { root : string
    ; session : P.Id.Session.t
    }
end

let read_persisted env paths =
  Eio.Switch.run (fun sw ->
    (* Reacquire real root and actor locks after the failed daemon switch joined.
       Historical explicit-close bookkeeping is not a crash recovery receipt. *)
    let nonce () = P.Id.Transaction.create () |> P.Id.Transaction.to_string in
    let store =
      Store.Session_store.open_existing
        ~env
        ~sw
        ~root:paths.Recovery_paths.root
        ~process_start_identity:None
        ~lock_nonce:(nonce ())
      |> Existing.store_ok
    in
    protect_cleanup
      ~finally:(fun () -> Store.Session_store.close store |> Existing.store_ok)
      (fun () ->
         let handle =
           Store.Session_store.open_session
             store
             ~sw
             ~actor_lock_nonce:(nonce ())
             paths.session
           |> Existing.store_ok
         in
         protect_cleanup
           ~finally:(fun () ->
             Store.Session_store.close_session store handle |> Existing.store_ok)
           (fun () ->
              let limits = D.Limits.default in
              let journal =
                Store.Journal.open_existing
                  ~env
                  ~directory:(Store.Session_store.Handle.journal_directory handle)
                  ~max_payload_length:(64 * 1024 * 1024)
                  ~max_segment_bytes:67108864L
                  ~max_segment_frames:4096
                |> Existing.store_ok
              in
              let recovered =
                Store.Recovery.read
                  ~env
                  ~journal
                  ~snapshot_directory:
                    (Store.Session_store.Handle.snapshot_directory handle)
                  ~max_snapshot_payload_length:(64 * 1024 * 1024)
                  ~session_id:paths.session
                  ~initial:None
                  ~restore_snapshot:(fun snapshot ->
                    Persistence.restore_snapshot ~limits snapshot
                    |> Result.map ~f:Option.some)
                  ~apply:(fun restored transaction ->
                    match restored with
                    | None ->
                      Error
                        (Store.Store_error.Corrupt
                           "known-ACK fixture has no snapshot basis")
                    | Some restored ->
                      Persistence.apply_transaction ~limits restored transaction
                      |> Result.map ~f:Option.some)
                  ~validate_transaction:(Persistence.validate_transaction ~limits)
                  ~validate:(function
                    | None ->
                      Error
                        (Store.Store_error.Corrupt
                           "known-ACK fixture has no restored state")
                    | Some restored -> Persistence.validate restored)
                |> Existing.store_ok
              in
              Option.value_exn recovered.state |> Persistence.Restored.state)))
;;

let rec failures = function
  | Eio.Exn.Multiple errors -> List.concat_map errors ~f:(fun (exn, _) -> failures exn)
  | exn -> [ exn ]
;;

let assert_retained (selected : Selection.t) (retained : A.Session_state.t) =
  (* The genuine legacy producer carries creator source/origin evidence, not
     the v1 Moderator_event ownership envelope. Neither record grants authority. *)
  assert (
    P.Id.Session.equal
      selected.producer_transaction.session_id
      selected.transaction.session_id);
  [%test_eq: int] selected.producer_transaction.generation selected.transaction.generation;
  assert (
    Int64.compare
      selected.producer_transaction.transaction_sequence
      selected.transaction.transaction_sequence
    < 0);
  assert (
    Int64.compare
      selected.producer_transaction.session_revision
      selected.transaction.session_revision
    < 0);
  assert (Option.is_none selected.schedule.ownership);
  (match selected.schedule.status with
   | Scheduled -> ()
   | Delivering | Delivered | Cancelled | Failed _ ->
     failwith "acknowledged independent producer was already consumed");
  let provenance = Option.value_exn selected.schedule.inference_provenance in
  let admission =
    P.Goal_episode.admission (Option.value_exn (P.Goal.episode (Goal.goal retained)))
  in
  let run = Goal_resume_runtime_tests.run retained admission in
  assert (P.Run_source.equal (P.Goal.source (Goal.goal retained)) run.source);
  assert (P.Run_source.equal run.source (P.Schedule_provenance.source provenance));
  assert (
    Option.equal
      P.Inference_origin.equal
      (Some (Run run.id))
      (P.Schedule_provenance.origin provenance));
  assert (P.Invocation.equal_observer run.source.observer selected.callback.context.source);
  assert (P.Id.Session.equal selected.transaction.session_id retained.identity.session_id);
  assert (
    List.exists
      (A.Goal_catalog.receipts (Option.value_exn retained.goal_catalog))
      ~f:(A.Goal_command_receipt.equal selected.receipt));
  assert (
    List.exists
      retained.moderator_executions
      ~f:(P.Moderator_execution.equal selected.callback));
  assert (
    List.exists retained.schedules ~f:(fun schedule ->
      Jsonaf.exactly_equal
        (P.Schedule.Storage.to_json selected.schedule)
        (P.Schedule.Storage.to_json schedule)));
  assert (
    P.Goal.State.equal
      (Open (Held (Needs_input Native.reason)))
      (P.Goal.state (Goal.goal retained)));
  [%test_eq: string]
    "progress after the exact Hold"
    (P.Goal.Progress.to_string (P.Goal.progress (Goal.goal retained)));
  [%test_eq: int64]
    (Int64.succ selected.receipt.goal_revision)
    (P.Goal.revision (Goal.goal retained));
  [%test_eq: int * int]
    (1, 0)
    (List.length retained.schedules, selected.schedule.delivery_count);
  assert (Option.is_none retained.active_operation);
  [%test_eq: int64 * int64]
    (0L, 0L)
    ( A.Goal_accounting.admitted_turns (Goal.accounting retained)
    , A.Goal_accounting.admitted_attempts (Goal.accounting retained) )
;;

let%expect_test
    "known control ACK survives original publication exception without Controlled success"
  =
  let root =
    Eio_main.run (fun env ->
      Mirage_crypto_rng_unix.use_default ();
      temporary_root env)
  in
  let fault = Fault.create () in
  let public_exception = ref None in
  let paths = ref None in
  let providers = ref 0 in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      incr providers;
      failwith "publication-failed control dispatched a provider")
  in
  (* Each host run owns its Eio scope; this outer cleanup runs between them. *)
  Exn.protect
    ~finally:(fun () ->
      Eio_main.run (fun env -> Eio.Path.rmtree Eio.Path.(Eio.Stdenv.fs env / root)))
    ~f:(fun () ->
      let escaped =
        match
          Creation_tests.with_daemon
            ~test_root:root
            ~wrap_env:(Fault.wrap fault)
            ~prompt_text:prompt
            ~qualify_chatml_extensions:true
            ~policy
            (fun env _sw _root daemon ->
               protect_cleanup
                 ~finally:(fun () -> fault.resume_clocks ())
                 (fun () ->
                    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
                      let rec await_poll_wait () =
                        match Daemon.For_testing.job_scheduler_wait_deadline daemon with
                        | Some _ -> ()
                        | None ->
                          Eio.Fiber.yield ();
                          await_poll_wait ()
                      in
                      await_poll_wait ();
                      Native.with_pending_entry daemon (fun entry session before ->
                        let handle = Option.value_exn entry.Registry.store_handle in
                        paths
                        := Some
                             { Recovery_paths.root =
                                 Store.Data_root.path
                                   (Store.Session_store.data_root (Daemon.store daemon))
                             ; session = before.identity.session_id
                             };
                        fault.cache_file
                        <- Some
                             (Filename.concat
                                (Store.Session_store.Handle.cache_directory handle)
                                "cache.bin");
                        (* Real daemon and retained-checkpoint preparation have joined. Pause the
                       injected clocks only now; the enclosing test timeout uses
                       the original environment and cleanup resumes before shutdown. *)
                        fault.pause_clocks ();
                        fault.session <- Some before.identity.session_id;
                        Eio.Cancel.protect (fun () ->
                          match
                            Service.start_episode
                              (Goal_resume_runtime_tests.public daemon)
                              ~entry
                              ~actor:original
                              ~session
                              ~request:(Native.request before Pending_cold)
                          with
                          | _ ->
                            failwith
                              "publication exception became a returned result or \
                               Controlled success"
                          | exception exn ->
                            public_exception := Some exn;
                            let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                            Exn.raise_with_original_backtrace exn backtrace)))))
        with
        | () -> failwith "known-ACK publication cut was not reached"
        | exception exn -> failures exn
      in
      assert (phys_equal (Option.value_exn !public_exception) fault.primary);
      assert (List.exists escaped ~f:(fun exn -> phys_equal exn fault.primary));
      [%test_eq: int * int] (1, 1) (fault.ack_hits, fault.publication_hits);
      assert (not fault.armed);
      (* This exact final cache publication follows actual partial runtime
          Root_binding.close and Inference_runtime.Session.close release joins. *)
      [%test_eq: int] 1 fault.partial_close_hits;
      [%test_eq: int] 0 !providers;
      let selected = Option.value_exn fault.selected in
      let retained =
        Eio_main.run (fun env -> read_persisted env (Option.value_exn !paths))
      in
      assert_retained selected retained);
  print_endline
    "earlier producer ACK precedes parent control/checkpoint ACK; genuine publication \
     throws original exception; partial constructor closes and physical reopen retains \
     facts without Controlled success";
  (* Fatal publication has closed the Actor before its queued cleanup can post.
     Preserve this exact visible diagnostic; do not claim graceful shutdown. *)
  [%expect
    {|
    +queued moderator cleanup failed: session command mailbox is closed
    earlier producer ACK precedes parent control/checkpoint ACK; genuine publication throws original exception; partial constructor closes and physical reopen retains facts without Controlled success
    |}]
;;

module Post_ack = struct
  type t =
    | Revoke_original
    | Cancel_caller
  [@@deriving sexp_of]
end

exception Post_ack_caller_cancelled of unit ref

let rejected_after_ack = function
  | Error error -> error
  | Ok (Service.Outcome.Attempted attempt) ->
    (match Service.Attempt.result attempt with
     | Error error | Ok (Activation_failed { error; _ }) -> error
     | Ok (Controlled _ | Settled _ | Activated _ | Retained _ | Admission_uncertain _) ->
       failwith "post-ACK owner boundary returned a positive or uncertain completion")
  | Ok (Retained _) -> failwith "post-ACK owner boundary retained a positive completion"
;;

let post_ack mode =
  let entered, enter = Eio.Promise.create () in
  let released, release = Eio.Promise.create () in
  let caller, caller_ready = Eio.Promise.create () in
  let release_gate () =
    if Option.is_none (Eio.Promise.peek released) then Eio.Promise.resolve release ()
  in
  let fault =
    Fault.create
      ~on_publication:(fun () ->
        Eio.Promise.resolve enter ();
        Eio.Promise.await released)
      ()
  in
  let live = ref true in
  let actor = Goal_resume_tests.guarded Retry_support.principal live in
  let sentinel = Post_ack_caller_cancelled (ref ()) in
  let providers = ref 0 in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      incr providers;
      failwith "post-ACK owner boundary dispatched a provider")
  in
  Creation_tests.with_daemon
    ~wrap_env:(Fault.wrap fault)
    ~prompt_text:prompt
    ~qualify_chatml_extensions:true
    ~policy
    (fun env _sw _root daemon ->
       protect_cleanup
         ~finally:(fun () ->
           release_gate ();
           fault.resume_clocks ())
         (fun () ->
            Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
              let rec await_poll_wait () =
                match Daemon.For_testing.job_scheduler_wait_deadline daemon with
                | Some _ -> ()
                | None ->
                  Eio.Fiber.yield ();
                  await_poll_wait ()
              in
              await_poll_wait ();
              Native.with_pending_entry daemon (fun entry session before ->
                fault.pause_clocks ();
                fault.session <- Some before.identity.session_id;
                let observed =
                  Eio.Switch.run (fun call_sw ->
                    let task =
                      Eio.Fiber.fork_promise ~sw:call_sw (fun () ->
                        Eio.Cancel.sub (fun context ->
                          Eio.Promise.resolve caller_ready context;
                          Service.start_episode
                            (Goal_resume_runtime_tests.public daemon)
                            ~entry
                            ~actor
                            ~session
                            ~request:(Native.request before Pending_cold)))
                    in
                    protect_cleanup ~finally:release_gate (fun () ->
                      Eio.Fiber.first
                        (fun () -> Eio.Promise.await entered)
                        (fun () ->
                           Eio.Promise.await_exn task
                           |> rejected_after_ack
                           |> fun error ->
                           raise_s
                             [%sexp
                               "public call rejected before known-ACK gate"
                             , (error : P.Error.t)]);
                      [%test_eq: int * int] (1, 1) (fault.ack_hits, fault.publication_hits);
                      assert (Option.is_some fault.selected);
                      (match mode with
                       | Post_ack.Revoke_original -> live := false
                       | Cancel_caller ->
                         Eio.Cancel.cancel (Eio.Promise.await caller) sentinel);
                      release_gate ();
                      Result.try_with (fun () -> Eio.Promise.await_exn task)))
                in
                (match mode, observed with
                 | Post_ack.Revoke_original, Ok result ->
                   let error = rejected_after_ack result in
                   [%test_eq: P.Error.code] Permission_denied error.code;
                   [%test_eq: string] "Run original authority expired" error.message;
                   assert (not error.retryable)
                 | Cancel_caller, Error (Eio.Cancel.Cancelled reason) ->
                   assert (phys_equal reason sentinel)
                 | Revoke_original, Error exn | Cancel_caller, Error exn -> raise exn
                 | Cancel_caller, Ok _ ->
                   failwith "cancelled caller returned a completion");
                let retained = Goal_resume_runtime_tests.checkpoint entry in
                let selected = Option.value_exn fault.selected in
                assert_retained selected retained;
                assert (
                  List.equal
                    P.History.equal_entry
                    before.conversation.canonical_history
                    retained.conversation.canonical_history);
                assert (not fault.armed);
                assert (not (Owner.is_loaded entry.runtime));
                assert (
                  A.Session_state.Runtime_initialization.equal
                    before.runtime_initialization
                    retained.runtime_initialization);
                assert (Goal_resume_runtime_tests.callbacks_complete retained);
                [%test_eq: int]
                  (List.length before.moderator_executions + 1)
                  (List.length retained.moderator_executions);
                let admission =
                  P.Goal_episode.admission
                    (Option.value_exn (P.Goal.episode (Goal.goal retained)))
                in
                let run = Goal_resume_runtime_tests.run retained admission in
                assert (List.is_empty (Goal_resume_runtime_tests.operation_work run));
                (match run.lifecycle with
                 | Admitted | Active | Waiting _ -> ()
                 | Terminal _ ->
                   failwith "post-ACK owner cleanup fabricated a Run terminal");
                assert (
                  A.Goal_accounting.equal
                    (Goal.accounting before)
                    (Goal.accounting retained));
                assert (
                  List.is_empty (A.Goal_accounting.bindings (Goal.accounting retained)));
                [%test_eq: int] 0 !providers))))
;;

let%expect_test
    "known control ACK retains facts across original revocation and caller cancellation"
  =
  List.iter [ Post_ack.Revoke_original; Cancel_caller ] ~f:post_ack;
  print_endline
    "known control ACK retains exact callback/producer/account; original revocation \
     rejects and actual caller cancellation propagates without input/provider or \
     fabricated interruption";
  [%expect
    {| known control ACK retains exact callback/producer/account; original revocation rejects and actual caller cancellation propagates without input/provider or fabricated interruption |}]
;;
