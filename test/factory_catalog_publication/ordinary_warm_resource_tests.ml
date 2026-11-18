open! Core
open Agent_server_test_support
open Retry_support
module Existing = Delegated_load_tests
module Warm = Delegated_warm_resource_tests
module B = A.Runtime_builder
module E = P.Moderator_execution
module Queue = A.Moderator_queue

let prompt =
  {|<authoring_context policy="manual"/><config model="fixture-model"/>
<developer>Actual initial ordinary root retained resources.</developer>
<script id="root_owner" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
| `Session_start -> Task.pure(state + 1)
| `Turn_end ->
  Task.bind(Schedule.after_ms(0, `String("598-foreground-ended")), fun ignored ->
    Task.pure(state + 1))
| `Internal_event(payload) ->
  (match payload with
   | `String("598-foreground-ended") -> Task.pure(state + 1)
   | _ -> Task.fail("unexpected ordinary root queue head"))
| _ -> Task.pure(state)
</script>|}
;;

module Polling = struct
  type t =
    { mutable pause : unit -> unit
    ; mutable resume : unit -> unit
    ; mutable wall_now : unit -> float
    ; mutable mono_now : unit -> Mtime.t
    }

  let create () =
    { pause = ignore
    ; resume = ignore
    ; wall_now = (fun () -> 0.)
    ; mono_now = (fun () -> Mtime.min_stamp)
    }
  ;;

  let wrap t (env : Eio_unix.Stdenv.base) =
    let clock, pause_wall, resume_wall, _ =
      controlled_wall_clock env#clock ~initial:(Eio.Time.now env#clock)
    in
    let mono_clock, pause_mono, resume_mono, _ =
      controlled_monotonic_clock env#mono_clock
    in
    t.pause
    <- (fun () ->
         pause_wall ();
         pause_mono ());
    t.resume
    <- (fun () ->
         resume_mono ();
         resume_wall ());
    t.wall_now <- (fun () -> Eio.Time.now clock);
    t.mono_now <- (fun () -> Eio.Time.Mono.now mono_clock);
    let env : Eio_unix.Stdenv.base =
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
    env
  ;;

  let park t daemon =
    t.pause ();
    let rec waiting () =
      match
        ( Daemon.For_testing.job_scheduler_wait_deadline daemon
        , Daemon.For_testing.schedule_scheduler_wait_deadline daemon )
      with
      | Some wall, Some mono
        when Float.(wall > t.wall_now ()) && Mtime.compare mono (t.mono_now ()) > 0 -> ()
      | _ ->
        Eio.Fiber.yield ();
        waiting ()
    in
    waiting ();
    Daemon.For_testing.await_schedule_scheduler_idle daemon
  ;;
end

module Resource_loan = struct
  type t =
    { released : unit Eio.Promise.t
    ; release : unit Eio.Promise.u
    ; entered : B.t Eio.Promise.t
    ; task : (unit Owner.Scope_outcome.primary, exn) result Eio.Promise.t
    }

  let create ~sw entry =
    let released, release = Eio.Promise.create () in
    let entered, enter = Eio.Promise.create () in
    let task =
      Eio.Fiber.fork_promise ~sw (fun () ->
        Existing.capture_load (fun () ->
          Owner.with_delegation_resources entry.Registry.runtime (fun runtime ->
            Eio.Promise.resolve enter runtime;
            Eio.Promise.await released;
            Ok ())))
    in
    { released; release; entered; task }
  ;;

  let await t = Warm.await_entry t.entered t.task

  let release t =
    if Option.is_none (Eio.Promise.peek t.released) then Eio.Promise.resolve t.release ()
  ;;

  let join t = Eio.Promise.await_exn t.task |> Existing.restore
end

let snapshot (current : A.Session_state.t) =
  A.Moderator_checkpoint.decode current.moderator |> protocol_ok |> Option.value_exn
;;

let queue current =
  A.Moderator_checkpoint.queue current.A.Session_state.moderator
  |> protocol_ok
  |> Option.value_exn
;;

let assert_live_source runtime =
  match A.Prompt_source_lease.availability runtime.B.source_lease with
  | Live -> ()
  | Missing_receipt | Unavailable _ | Revoked | Closed ->
    failwith "initial ordinary source owner was not retained live"
;;

let assert_same_resources previous current =
  Warm.assert_same_resources previous current;
  assert_live_source current;
  assert (
    phys_equal
      (Inference_client.Execution.context previous.B.inference_execution)
      (Inference_client.Execution.context current.B.inference_execution));
  (* This captured parser owns the original Ctx/history allocator. A new binding
     must not construct another parser/allocator merely because its worker is new. *)
  assert (phys_equal previous.parse_user_content current.parse_user_content)
;;

let assert_no_reconstruction first_identity before after counters ports =
  assert (Jsonaf.exactly_equal first_identity (Warm.identity (Warm.loaded after)));
  let current = Warm.settled after in
  assert (P.Run_source.equal (Warm.source before) (Warm.source current));
  assert (Option.equal Jsonaf.exactly_equal before.moderator current.moderator);
  assert (List.equal E.equal before.moderator_executions current.moderator_executions);
  [%test_eq: int64]
    before.conversation.next_history_sequence
    current.conversation.next_history_sequence;
  [%test_eq: int64]
    before.conversation.reserved_history_through
    current.conversation.reserved_history_through;
  [%test_eq: int] ports !(counters.inference_ports);
  [%test_eq: int] 0 !(counters.providers);
  match current.runtime_initialization with
  | Ready -> ()
  | Pending _ -> failwith "ordinary warm binding replayed initialization"
;;

module Boundary = struct
  type t =
    { env : Eio_unix.Stdenv.base
    ; phase : string ref
    }

  let within t name f =
    t.phase := name;
    Eio.Time.with_timeout_exn t.env#clock 10. f
  ;;

  let join_all t tasks =
    let failure = ref None in
    List.iter tasks ~f:(fun (name, join) ->
      try within t name join with
      | exn ->
        if Option.is_none !failure
        then failure := Some (exn, Stdlib.Printexc.get_raw_backtrace ()));
    Option.iter !failure ~f:(fun (exn, backtrace) ->
      Exn.raise_with_original_backtrace exn backtrace)
  ;;
end

let with_fixture ?(wrap_env = Fn.id) io f =
  let polling = Polling.create () in
  let phase = ref "initial public root creation" in
  with_recoverable_fixture
    ~timeout:90.
    ~prompt_text:prompt
    ~wrap_env:(fun env -> Warm.Handoff_io.wrap io (Polling.wrap polling (wrap_env env)))
    (fun env sw _root daemon _policy entry first revision counters ->
       let boundary = { Boundary.env :> Eio_unix.Stdenv.base; phase } in
       let within name f = Boundary.within boundary name f in
       protect_cleanup
         ~finally:(fun () -> polling.resume ())
         (fun () ->
            try
              within "park actual scheduler inputs and join callbacks" (fun () ->
                Polling.park polling daemon);
              let initial = Warm.settled entry in
              assert (Option.is_none initial.spec.delegation);
              assert (Option.is_none initial.active_operation);
              assert (List.is_empty initial.jobs);
              assert (List.is_empty initial.schedules);
              assert (
                Option.for_all initial.run_state ~f:(fun index ->
                  List.is_empty (A.Run_state.job_deliveries index)));
              [%test_eq: int] 1 (List.length (Warm.completed initial Session_start));
              [%test_eq: int] 0 (List.length (Warm.completed initial Session_resume));
              assert (List.is_empty (snapshot initial).queued_internal_events);
              [%test_eq: int] 0 !(counters.providers);
              assert (Owner.is_loaded entry.runtime);
              assert_live_source first;
              io.record
              <- Some
                   (Eio.Path.native_exn
                      Eio.Path.(
                        A.Prompt_revision.materialized_tree revision
                        / A.Prompt_revision.root_relative_path revision));
              io.cache
              <- Some
                   (Filename.concat
                      (Agent_store.Session_store.Handle.cache_directory
                         (Option.value_exn entry.store_handle))
                      "cache.bin");
              let writer, _ =
                Actor.attach entry.actor ~mode:Read_write ~subscribe:false |> protocol_ok
              in
              protect_cleanup
                ~finally:(fun () -> Actor.detach entry.actor writer.id |> protocol_ok)
                (fun () -> f env sw daemon entry first initial writer counters boundary)
            with
            | exn ->
              let backtrace = Stdlib.Printexc.get_raw_backtrace () in
              (try Eio.traceln "598 ordinary warm phase: %s" !phase with
               | _ -> ());
              Exn.raise_with_original_backtrace exn backtrace))
;;

let await_load io loading =
  Eio.Fiber.first
    (fun () -> Eio.Promise.await io.Warm.Handoff_io.entered)
    (fun () ->
       Eio.Promise.await_exn loading |> Existing.restore;
       failwith "ordinary warm load returned before the verified artifact read")
;;

(* Stop's committed-state host retires the selected source lease. The exact
   lease cancellation and the original-basis Conflict are both lawful refusal;
   no other cancellation or returned error is expected. *)
let expect_stopped_selection = function
  | Owner.Scope_outcome.Returned (Error { P.Error.code = Conflict; _ }) -> ()
  | Owner.Scope_outcome.Raised (Eio.Cancel.Cancelled Stdlib.Exit, _) -> ()
  | outcome ->
    Existing.restore outcome;
    failwith "Stop-selected attachment was installed"
;;

let assert_admin_excluded entry =
  let before = Warm.settled entry in
  let runtime = Warm.loaded entry in
  let identity = Warm.identity runtime in
  let ran = ref false in
  Owner.with_administration entry.runtime (fun () ->
    ran := true;
    Ok ())
  |> Warm.expect_busy;
  assert (not !ran);
  Source_preparation_tests.assert_same before entry;
  assert (phys_equal runtime.worker (Warm.loaded entry).worker);
  assert (Jsonaf.exactly_equal identity (Warm.identity (Warm.loaded entry)))
;;

let%expect_test
    "fresh ordinary root retains its initial Product across every warm ingress"
  =
  let io = Warm.Handoff_io.create () in
  with_fixture io (fun _env sw _daemon entry first initial writer counters boundary ->
    let within name f = Boundary.within boundary name f in
    let ports = !(counters.inference_ports) in
    let first_identity = Warm.identity first in
    let first_loan = Resource_loan.create ~sw entry in
    let later_loan = ref None in
    let loading = ref None in
    protect_cleanup
      ~finally:(fun () ->
        Warm.Handoff_io.release io;
        Resource_loan.release first_loan;
        Option.iter !later_loan ~f:Resource_loan.release;
        Boundary.join_all
          boundary
          [ ( "join selected load cleanup"
            , fun () ->
                Option.iter !loading ~f:(fun task ->
                  Eio.Promise.await_exn task |> Existing.restore) )
          ; ("join initial resource cleanup", fun () -> Resource_loan.join first_loan)
          ; ( "join later resource cleanup"
            , fun () -> Option.iter !later_loan ~f:Resource_loan.join )
          ])
      (fun () ->
         let actual =
           within "retain initial public Product" (fun () ->
             Resource_loan.await first_loan)
         in
         assert (phys_equal first.worker actual.worker);
         assert_admin_excluded entry;
         within "detach first binding under real loan" (fun () ->
           Owner.unload_and_wait entry.runtime |> protocol_ok);
         assert (not (Owner.is_loaded entry.runtime));
         assert (not (Owner.source_lane_idle entry.runtime));
         [%test_eq: int] 0 io.cache_renames;
         let before = Warm.settled entry in
         io.armed <- true;
         let selected =
           Eio.Fiber.fork_promise ~sw (fun () ->
             Eio.Fiber.with_binding io.caller () (fun () ->
               Existing.capture_load (fun () ->
                 Owner.ensure_loaded_authenticated entry.runtime ~actor:original)))
         in
         loading := Some selected;
         within "verified authored source read after Product pin" (fun () ->
           await_load io selected);
         within "responsive selected source owner" (fun () ->
           assert (not (Owner.is_loaded entry.runtime));
           assert (not (Owner.source_lane_idle entry.runtime)));
         Resource_loan.release first_loan;
         within "former last resource borrower departure" (fun () ->
           Resource_loan.join first_loan);
         assert (Option.is_none (Eio.Promise.peek selected));
         [%test_eq: int] 1 io.reads;
         [%test_eq: int] 0 io.cache_renames;
         Warm.Handoff_io.release io;
         within "authenticated first warm install" (fun () ->
           Eio.Promise.await_exn selected |> Existing.restore);
         let second = Warm.loaded entry in
         assert_same_resources first second;
         assert_no_reconstruction first_identity before entry counters ports;
         assert (Owner.source_lane_idle entry.runtime);
         let retained = Resource_loan.create ~sw entry in
         later_loan := Some retained;
         let actual =
           within "retain installed warm Product" (fun () -> Resource_loan.await retained)
         in
         assert (phys_equal second.worker actual.worker);
         assert_admin_excluded entry;
         within "detach before lease ingress" (fun () ->
           Owner.unload_and_wait entry.runtime |> protocol_ok);
         let entered_background = ref false in
         within "background lease warm ingress" (fun () ->
           Owner.with_background_runtime entry.runtime (fun runtime ->
             assert_same_resources second runtime;
             entered_background := true;
             Ok ())
           |> protocol_ok);
         assert !entered_background;
         let third = Warm.loaded entry in
         assert_same_resources second third;
         assert_no_reconstruction first_identity before entry counters ports;
         within "detach before locked ingress" (fun () ->
           Owner.unload_and_wait entry.runtime |> protocol_ok);
         within "existing locked ingress warm install" (fun () ->
           Owner.For_testing.with_loaded_runtime entry.runtime (fun () -> Ok ())
           |> protocol_ok);
         let fourth = Warm.loaded entry in
         assert_same_resources third fourth;
         assert_no_reconstruction first_identity before entry counters ports;
         [%test_eq: int] 0 io.cache_renames;
         let current = Warm.settled entry in
         let message =
           Owner.parse_user_content
             entry.runtime
             ~id:(A.History_id_source.allocate entry.history_ids |> protocol_ok)
             { kind = Plain_text
             ; text = "598 actual ordinary retained worker."
             ; attachments = []
             }
           |> protocol_ok
           |> A.History_codec.to_protocol
         in
         let submission =
           Actor.submit_message_authenticated
             entry.actor
             ~actor:original
             ~attachment_id:writer.id
             message
           |> protocol_ok
         in
         let turns =
           P.Inference_query.Summary.turns
             (A.Inference_ledger.summary current.inference_ledger)
         in
         let ended =
           within "actual new worker foreground completion" (fun () ->
             Warm.await_turn
               entry
               counters.providers
               turns.completed
               (Option.value_exn submission.operation_id))
         in
         assert (List.is_empty (Warm.completed initial Internal_event));
         (match
            List.filter ended.conversation.canonical_history ~f:(fun entry ->
              P.History.Id.equal entry.P.History.id message.id)
          with
          | [ actual ] -> assert (P.History.equal_entry message actual)
          | _ -> failwith "actual ordinary user entry was lost or duplicated");
         assert (
           List.exists ended.conversation.canonical_history ~f:(fun entry ->
             P.History.equal_role entry.P.History.role Assistant
             && String.is_substring
                  (Jsonaf.to_string entry.payload)
                  ~substring:"539 retry complete"));
         assert (
           List.for_all current.conversation.canonical_history ~f:(fun prior ->
             List.exists
               ended.conversation.canonical_history
               ~f:(P.History.equal_entry prior)));
         (* The real foreground safe point drains emitted Internal_events.
            An owned timer is created by this exact Turn_end callback, then its
            authentic delivery is enqueued only after the worker has completed. *)
         assert (List.is_empty (snapshot ended).queued_internal_events);
         let ending = List.hd_exn (Warm.completed ended Turn_end) in
         assert (
           Option.exists
             ending.context.operation_id
             ~f:(P.Id.Operation.equal (Option.value_exn submission.operation_id)));
         let observed, due = Actor.due_schedules entry.actor |> protocol_ok in
         (match observed with
          | Idle -> ()
          | Stopped
          | Queued_for_slot
          | Starting
          | Recovering
          | Running_turn _
          | Compacting _
          | Waiting_for_permission _
          | Stopping
          | Failed _ ->
            failwith "completed foreground did not leave the timer runnable idle");
         let timer =
           match due with
           | [ timer ] -> timer
           | _ -> failwith "Turn_end did not create exactly one real due timer"
         in
         assert (Jsonaf.exactly_equal timer.payload (`String "598-foreground-ended"));
         let ownership = Option.value_exn timer.ownership in
         (match ownership.creator with
          | Moderator_event id ->
            assert (P.Id.Moderator_execution.equal id ending.context.id)
          | Invocation _ -> failwith "pending timer did not belong to actual Turn_end");
         assert (P.Invocation.equal_observer ownership.source ending.context.source);
         let provenance = Option.value_exn timer.inference_provenance in
         assert (
           P.Run_source.equal
             (P.Schedule_provenance.source provenance)
             (Warm.source ended));
         assert (
           Option.equal
             P.Inference_origin.equal
             (P.Schedule_provenance.origin provenance)
             (Some Unbound));
         let claimed =
           Actor.claim_schedule
             entry.actor
             ~schedule_id:timer.id
             ~generation:timer.generation
           |> protocol_ok
           |> Option.value_exn
         in
         within "enqueue authentic owned timer delivery after foreground" (fun () ->
           Owner.deliver_schedule entry.runtime claimed |> protocol_ok);
         let pending = Warm.settled entry in
         [%test_eq: int] 1 (List.length (snapshot pending).queued_internal_events);
         let emitted = queue pending in
         let head =
           match Queue.entries emitted with
           | [ head ] -> head
           | _ -> failwith "one actual timer delivery did not append one pending head"
         in
         (match Queue.Entry.producer head with
          | Some (A.Queue_producer.Schedule_delivery actual) ->
            assert (P.Id.Schedule.equal timer.id actual.id);
            [%test_eq: int] timer.generation actual.generation;
            [%test_eq: int] 1 actual.delivery_count
          | Some
              ( Event _
              | Invocation _
              | Invocation_at_source _
              | Job_completion _
              | Ingress_delivery _
              | Source_setup _ )
          | None -> failwith "pending head lost its actual Schedule delivery producer");
         assert (P.Run_source.equal (Queue.source emitted) (Warm.source ended));
         assert (
           Option.equal
             P.Inference_origin.equal
             (Queue.Entry.origin head)
             (P.Schedule_provenance.origin provenance));
         within "detach before pending idle ingress" (fun () ->
           Owner.unload_and_wait entry.runtime |> protocol_ok);
         within "actual pending idle warm ingress" (fun () ->
           Owner.drain_idle_moderator entry.runtime |> protocol_ok |> ignore);
         let fifth = Warm.loaded entry in
         assert_same_resources fourth fifth;
         let consumed = Warm.settled entry in
         assert (List.is_empty (snapshot consumed).queued_internal_events);
         let occurrence =
           match Warm.completed consumed Internal_event with
           | [ receipt ] -> Option.value_exn receipt.queue_occurrence
           | _ -> failwith "one actual root head was not consumed exactly once"
         in
         assert (
           P.Run_source.equal
             (Queue.source emitted)
             (E.Queue_occurrence.source occurrence));
         [%test_eq: int64]
           (Queue.Entry.ordinal head)
           (E.Queue_occurrence.ordinal occurrence);
         assert (
           Option.equal
             P.Inference_origin.equal
             (Queue.Entry.origin head)
             (E.Queue_occurrence.origin occurrence));
         let after_queue = queue consumed in
         [%test_eq: int64]
           (Int64.succ (Queue.head_ordinal emitted))
           (Queue.head_ordinal after_queue);
         [%test_eq: int64] (Queue.next_ordinal emitted) (Queue.next_ordinal after_queue);
         [%test_eq: int] ports !(counters.inference_ports);
         [%test_eq: int] 1 !(counters.providers);
         assert (P.Run_source.equal (Warm.source initial) (Warm.source consumed));
         [%test_eq: int] 1 (List.length (Warm.completed consumed Session_start));
         [%test_eq: int] 0 (List.length (Warm.completed consumed Session_resume));
         within "detach before no-work idle" (fun () ->
           Owner.unload_and_wait entry.runtime |> protocol_ok);
         let idle = Warm.settled entry in
         within "unloaded no-work idle does not construct" (fun () ->
           assert (not (Owner.drain_idle_moderator entry.runtime |> protocol_ok)));
         assert (not (Owner.is_loaded entry.runtime));
         Source_preparation_tests.assert_same idle entry;
         [%test_eq: int] ports !(counters.inference_ports);
         [%test_eq: int] 1 !(counters.providers);
         [%test_eq: int] 0 io.cache_renames;
         Resource_loan.release retained;
         within "last actual Product release" (fun () -> Resource_loan.join retained);
         [%test_eq: int] 1 io.cache_renames;
         assert (Owner.source_lane_idle entry.runtime)));
  [%test_eq: int] 1 io.cache_renames;
  print_endline
    "initial root Product preserved across four warm ingresses; real head consumed once; \
     no-work stayed unloaded; physical close once";
  [%expect
    {| initial root Product preserved across four warm ingresses; real head consumed once; no-work stayed unloaded; physical close once |}]
;;

let%expect_test
    "ordinary selected warm Stop refuses stale install then fresh basis reuses same \
     Product"
  =
  let io = Warm.Handoff_io.create () in
  with_fixture io (fun _env sw _daemon entry first _initial writer counters boundary ->
    let within name f = Boundary.within boundary name f in
    let ports = !(counters.inference_ports) in
    let first_identity = Warm.identity first in
    let loan = Resource_loan.create ~sw entry in
    let loading = ref None in
    protect_cleanup
      ~finally:(fun () ->
        Warm.Handoff_io.release io;
        Resource_loan.release loan;
        Boundary.join_all
          boundary
          [ ( "join stale selected load cleanup"
            , fun () ->
                Option.iter !loading ~f:(fun task ->
                  Eio.Promise.await_exn task |> expect_stopped_selection) )
          ; ("join stopped resource cleanup", fun () -> Resource_loan.join loan)
          ])
      (fun () ->
         let actual =
           within "retain second initial Product" (fun () -> Resource_loan.await loan)
         in
         assert (phys_equal first.worker actual.worker);
         within "detach before Stop-selected load" (fun () ->
           Owner.unload_and_wait entry.runtime |> protocol_ok);
         let before = Warm.settled entry in
         let original_basis =
           Actor.Ordinary_attachment_basis.capture before |> protocol_ok
         in
         io.armed <- true;
         let selected =
           Eio.Fiber.fork_promise ~sw (fun () ->
             Eio.Fiber.with_binding io.caller () (fun () ->
               Existing.capture_load (fun () ->
                 Owner.ensure_loaded_authenticated entry.runtime ~actor:original)))
         in
         loading := Some selected;
         within "second selected authored read" (fun () -> await_load io selected);
         Actor.stop entry.actor ~attachment_id:writer.id ~mode:Cancel
         |> protocol_ok
         |> ignore;
         assert (not (P.Session.equal_desired_state before.lifecycle.desired Stopped));
         [%test_eq: int] 0 io.cache_renames;
         Warm.Handoff_io.release io;
         within "stale ordinary attachment rejected after actual Stop" (fun () ->
           Eio.Promise.await_exn selected |> expect_stopped_selection);
         (* The real Stop publication forks an unload. Join the same Owner's
            retirement before selecting a new lawful stopped-session basis. *)
         within "join actual Stop-triggered retirement" (fun () ->
           Owner.unload_and_wait entry.runtime |> protocol_ok);
         let stopped = Warm.settled entry in
         assert (P.Session.equal_desired_state stopped.lifecycle.desired Stopped);
         assert (not (Owner.is_loaded entry.runtime));
         (* Source-stage cancellation is not itself evidence that the final
            Actor install rejects the original source/Stop basis. Exercise that
            negative separately, without performing any successful direct install. *)
         within "actual Actor rejects original stopped attachment basis" (fun () ->
           Actor.check_runtime_admission entry.actor |> protocol_ok;
           let expected =
             match Actor.Ordinary_attachment_basis.check original_basis stopped with
             | Error failure ->
               [%test_eq: P.Error.code] Conflict failure.code;
               failure
             | Ok () -> failwith "actual Stop did not invalidate its original basis"
           in
           match
             Actor.set_runtime_worker
               ~actor:original
               ~moderator_manager:(Option.value_exn first.moderator_manager)
               ~expected_ordinary:original_basis
               entry.actor
               ~worker:(Some first.worker)
               ~inference:(Some first.inference_execution)
           with
           | Error failure ->
             assert (
               Jsonaf.exactly_equal (P.Error.to_json expected) (P.Error.to_json failure))
           | Ok () -> failwith "actual Actor installed the stale ordinary root binding");
         Source_preparation_tests.assert_same stopped entry;
         assert (not (Owner.is_loaded entry.runtime));
         assert (Jsonaf.exactly_equal first_identity (Warm.identity first));
         [%test_eq: int] ports !(counters.inference_ports);
         [%test_eq: int] 0 !(counters.providers);
         [%test_eq: int] 0 io.cache_renames;
         within "fresh lawful stopped-root basis" (fun () ->
           Owner.ensure_loaded_authenticated entry.runtime ~actor:original |> protocol_ok);
         let fresh = Warm.loaded entry in
         assert_same_resources first fresh;
         assert_no_reconstruction first_identity stopped entry counters ports;
         assert_admin_excluded entry;
         within "detach fresh lawful stopped binding" (fun () ->
           Owner.unload_and_wait entry.runtime |> protocol_ok);
         Resource_loan.release loan;
         within "release original stopped Product" (fun () -> Resource_loan.join loan);
         [%test_eq: int] 1 io.cache_renames));
  [%test_eq: int] 1 io.cache_renames;
  print_endline
    "actual Stop fenced selected root binding; fresh lawful basis reused same Product; \
     no cold replay; physical close once";
  [%expect
    {| actual Stop fenced selected root binding; fresh lawful basis reused same Product; no cold replay; physical close once |}]
;;
