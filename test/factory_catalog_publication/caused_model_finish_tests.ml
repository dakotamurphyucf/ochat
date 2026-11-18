open! Core
open Agent_server_test_support
open Retry_support
module S = Caused_async_support
module Graph = Agent_server.Graph_tracking
module O = Inference.Observation
module Ledger = S.Ledger
module Model = S.Model
module PC = S.PC
module Book = S.Book
module Host = A.Inference_graph_owner.Host
module Facts = A.Inference_graph_owner.Factual_usage
module Read = Delegated_policy_cleanup_tests.Root_read

let recipe = "614-finish-model.chatmd"
let effect_file = "614-finish-async.txt"
let marker = "614_ACTUAL_MODEL_FINISH"

let parent_prompt =
  {|<developer>Actual caused Model native finish and factual retry.</developer>
<tool name="finish_recipe" agent="614-finish-model.chatmd" local/>
<script id="finish-policy" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
| `Pre_tool_call(p) ->
  if (if state == 0 then p.name == "read_file" else false) then
    let* job = Tool.spawn("read_file", `Object([
      {key = "root"; value = `String("data")},
      {key = "file"; value = `String("614-finish-async.txt")}
    ])) in Task.pure(state + 1)
  else Task.pure(state)
| _ -> Task.pure(state)
</script>|}
;;

let ledger_json current =
  Ledger.to_document current.A.Session_state.inference_ledger
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (Ledger.Error.sexp_of_t error))
  |> Result.ok_or_failwith
  |> Document_schema.Document.json
;;

let book_json current =
  Option.value_exn current.A.Session_state.goal_policy_contributions
  |> Book.to_document
  |> protocol_ok
  |> Document_schema.Document.json
;;

let projection_json projection = PC.Projection.to_json projection
let native_ack owner = Host.Native_join.current owner |> protocol_ok |> Option.value_exn

let actual_facts owner handle =
  Host.retained_registration owner handle
  |> Option.value_exn
  |> A.Inference_graph_owner.Registration.factual_usage
  |> Option.value_exn
;;

let assert_sealed owner =
  match Host.phase owner with
  | Sealed -> ()
  | Open | Finished -> failwith "failed finish lost the actual sealed graph"
;;

let assert_finished owner =
  match Host.phase owner with
  | Finished -> ()
  | Open | Sealed -> failwith "successful finish did not clear the actual graph"
;;

let model_job fixture =
  let subject = fixture.S.probe in
  Actor.with_job_execution
    subject.parent.actor
    ~job_id:subject.job.id
    ~generation:subject.job.generation
    ~attempt:subject.job.attempt
    ~deadline:None
    (fun execution ->
       let current = state subject.parent in
       let candidate : P.Job.t =
         { id = P.Id.Job.create ()
         ; session_id = current.identity.session_id
         ; generation = current.identity.generation
         ; kind = Model_call
         ; payload =
             `Object
               [ "recipe", `String Chat_response.Model_executor.agent_prompt_v1_name
               ; ( "payload"
                 , `Object
                     [ "prompt", `String recipe
                     ; "input", `String marker
                     ; "is_local", `True
                     ] )
               ]
         ; status = Queued
         ; retry_policy = Never
         ; attempt = 0
         ; created_at =
             P.Timestamp.of_time_ns
               (Time_ns.of_span_since_epoch (Time_ns.Span.of_sec (fixture.clock_now ())))
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
       A.Model_job_admission.admit execution.model_jobs candidate)
  |> protocol_ok
;;

let assert_target subject job expected =
  let binding =
    List.filter (state subject.S.parent).model_job_targets ~f:(fun binding ->
      P.Id.Job.equal job.P.Job.id (A.Model_job_target.job_id binding)
      && Int.equal job.generation (A.Model_job_target.generation binding))
    |> Model.only "actual finish Model source and execution binding"
  in
  List.iter
    [ A.Model_job_target.source binding; A.Model_job_target.execution binding ]
    ~f:(fun selection ->
      match Inference.Selection.view selection with
      | Captured target -> assert (Inference.Request.Target.equal expected target)
      | Unresolved -> failwith "actual Model inference bypassed target capture")
;;

let native_terminal current job =
  match A.Goal_work_attribution.run_of_job current job |> protocol_ok with
  | None ->
    assert (
      Option.equal P.Inference_origin.equal (Some Unbound) job.P.Job.inference_origin)
  | Some run ->
    let work = A.Run_job_occurrence.work job |> protocol_ok in
    assert (List.mem run.owned_work work ~equal:P.Run_work.equal);
    List.filter run.terminal_work ~f:(fun proof ->
      P.Run_work.equal work proof.P.Run_work.Terminal.work)
    |> Model.only "actual finished Model terminal proof"
    |> fun proof ->
    assert (P.Run_work.Terminal.equal_outcome Succeeded proof.P.Run_work.Terminal.outcome)
;;

let%expect_test "actual Model native join ACK survives failed remote correction and retry"
  =
  let read = Read.create () in
  let selected_job = ref None in
  let pause_mono = ref ignore
  and resume_mono = ref ignore
  and mono_now = ref (fun () -> failwith "finish scheduler clock absent") in
  let at_direct subject =
    let job = Option.value_exn !selected_job in
    Caused_async_retry_tests.direct_parent subject job;
    S.observe_attempt subject job |> ignore
  in
  S.with_claimed_async
    ~wrap_env:(fun env ->
      let mono_clock, pause, resume, _ = controlled_monotonic_clock env#mono_clock in
      pause_mono := pause;
      resume_mono := resume;
      (mono_now := fun () -> Eio.Time.Mono.now mono_clock);
      Read.wrap
        read
        (object
           method clock = env#clock
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
         : Eio_unix.Stdenv.base))
    ~at_direct
    ~scenario:S.Model_finish_cleanup
    ~parent_prompt_text:parent_prompt
    ~sources:
      [ S.original_file, "actual child read"
      ; effect_file, "actual Async native finish"
      ; recipe, "<developer>Actual selected Model source.</developer>"
      ]
    ~model_marker:marker
    ~at_model:(fun _ _ -> failwith "finish fixture unexpectedly executed a Model recipe")
    (fun env sw daemon fixture ->
       let subject = fixture.probe in
       let within phase f = Model.within env phase f in
       let born = within "actual Model admission" (fun () -> model_job fixture) in
       Caused_async_retry_tests.direct_parent subject born;
       let job =
         within "actual Model claim" (fun () ->
           Actor.claim_job
             subject.parent.actor
             ~job_id:born.id
             ~generation:born.generation
           |> protocol_ok
           |> Option.value_exn)
       in
       [%test_eq: int] 1 job.attempt;
       assert (P.Job.equal_delivery Not_required job.delivery);
       selected_job := Some job;
       let inference =
         Factory.model_job_inference (Daemon.factory daemon) subject.parent job
         |> protocol_ok
       in
       let target = Inference_runtime.Context.target inference.context in
       within "actual Model recipe target ACK" (fun () ->
         inference.capture_recipe_target target |> protocol_ok);
       assert_target subject job target;
       let request =
         Inference.Request.create
           ~target
           ~history:[]
           ~tools:[]
           ~assets:[]
           ~limits:Transcript.Admission.default
         |> Result.map_error ~f:(fun error ->
           Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))
         |> Result.ok_or_failwith
       in
       let owner = ref None in
       let admissions = ref 0 in
       let usage = ref [] in
       let graph =
         Graph.create
           subject.parent.actor
           ~source:
             (Transcript.Source_id.of_string "614-actual-model-finish"
              |> Result.ok_or_failwith)
           ~upstream:
             { new_preparation_id = (fun () -> "614-actual-model-finish-preparation")
             ; on_admitted = (fun ~scope:_ ~accounting_id:_ -> Int.incr admissions)
             ; on_attempt = ignore
             ; on_observation = ignore
             ; on_completion = ignore
             }
         |> protocol_ok
       in
       let receipt =
         protect_cleanup
           ~finally:(fun () ->
             (* Resume before protected Graph/native/child cleanup joins. *)
             !resume_mono ();
             read.selected <- None;
             within "join exact owned finish graph" (fun () ->
               Graph.seal graph |> protocol_ok;
               Graph.finish graph |> protocol_ok))
           (fun () ->
              Actor.with_job_execution
                subject.parent.actor
                ~job_id:job.id
                ~generation:job.generation
                ~attempt:job.attempt
                ~deadline:None
                (fun execution ->
                   let allocate ~graph ~relation ~configuration =
                     let open Result.Let_syntax in
                     let%map allocation =
                       execution.allocate_inference ~graph ~relation ~configuration
                     in
                     assert (Option.is_none !owner);
                     owner := Some graph;
                     allocation
                   in
                   let identity = Graph.bind_identity graph ~allocate in
                   let receipt =
                     Exn.protect
                       ~finally:(fun () -> fixture.direct := false)
                       ~f:(fun () ->
                         fixture.direct := true;
                         within "one actual Model provider" (fun () ->
                           Inference_client.run
                             inference.context
                             ~sw
                             ~identity
                             ~relation:Root
                             ~request
                             ~before_dispatch:ignore
                             ~on_attempt:(Graph.on_attempt graph)
                             ~on_observation:(fun incoming ->
                               match O.payload incoming with
                               | Usage _ -> usage := !usage @ [ incoming ]
                               | Configuration _
                               | Context_estimate _
                               | Transport_selection _
                               | Diagnostic _ -> Graph.on_observation graph incoming)
                             ~on_completion:(Graph.on_completion graph)
                             ~on_event:ignore)
                         |> Result.map_error ~f:(fun error ->
                           Sexp.to_string_hum (Inference_client.Error.sexp_of_t error))
                         |> Result.ok_or_failwith)
                   in
                   S.Failure.raise_if_present fixture.failure;
                   [%test_eq: int] 1 !admissions;
                   [%test_eq: int] 1 !(fixture.direct_providers);
                   let owner = Option.value_exn !owner in
                   assert (Option.is_none (Host.Native_join.current owner |> protocol_ok));
                   let observed = Model.only "one actual Model H" subject.observed in
                   let handle = observed.handle in
                   let facts = actual_facts owner handle in
                   assert (Facts.Host.initial_confirmed facts |> protocol_ok);
                   (* Graph.finish owns this late correction/retry cut. Park and
                      join independent idle sweepers before the actual Usage ACK,
                      after provider work returned and the initial import was confirmed. *)
                   !pause_mono ();
                   within "finish actual maintenance schedulers parked" (fun () ->
                     let rec await () =
                       S.Failure.raise_if_present fixture.failure;
                       match
                         ( Daemon.For_testing.job_scheduler_wait_deadline daemon
                         , Daemon.For_testing.schedule_scheduler_wait_deadline daemon )
                       with
                       | Some wall, Some mono
                         when Float.(wall > fixture.clock_now ())
                              && Mtime.compare mono (!mono_now ()) > 0 -> ()
                       | Some _, Some _ | Some _, None | None, Some _ | None, None ->
                         Eio.Fiber.yield ();
                         await ()
                     in
                     await ();
                     Daemon.For_testing.await_schedule_scheduler_idle daemon);
                   let before_late = state subject.parent in
                   let completed = S.row before_late handle in
                   S.assert_completed completed;
                   assert (
                     not
                       (List.exists
                          (O.Attempt_record.observations (Ledger.Row.record completed))
                          ~f:(fun incoming ->
                            match O.payload incoming with
                            | Usage _ -> true
                            | Configuration _
                            | Context_estimate _
                            | Transport_selection _
                            | Diagnostic _ -> false)));
                   let incoming = Model.only "actual delayed provider Usage" !usage in
                   assert (O.equal incoming (Inference_runtime.Receipt.usage receipt));
                   assert (
                     Transcript.Scope.equal
                       (Ledger.Handle.scope handle)
                       (O.scope incoming));
                   assert (
                     O.Observation_id.equal
                       (Ledger.Handle.accounting_id handle)
                       (O.id incoming));
                   let before_remote = state subject.child in
                   let old_projection = Model.pending_projection before_remote in
                   within "ACK actual delayed retained-row Usage" (fun () ->
                     Actor.observe_owned_inference subject.parent.actor ~owner incoming
                     |> protocol_ok);
                   let corrected = state subject.parent in
                   [%test_eq: int64]
                     Int64.(Ledger.revision before_late.inference_ledger + 1L)
                     (Ledger.revision corrected.inference_ledger);
                   [%test_eq: int64]
                     Int64.(before_late.counters.revision + 1L)
                     corrected.counters.revision;
                   let expected_projection =
                     PC.projections
                       (Option.value_exn corrected.policy_canonical_usage)
                       ~ledger:corrected.inference_ledger
                       ~session_revision:corrected.counters.revision
                     |> protocol_ok
                     |> List.filter ~f:(fun projection ->
                       PC.Stream.equal observed.stream (PC.Projection.stream projection))
                     |> Model.only "same actual causal stream after late ACK"
                   in
                   assert (
                     Int64.(
                       PC.Projection.session_revision expected_projection
                       > PC.Projection.session_revision old_projection));
                   [%test_eq: int64]
                     (PC.Projection.admitted_attempts old_projection)
                     (PC.Projection.admitted_attempts expected_projection);
                   assert (Facts.Host.pending facts |> protocol_ok);
                   Graph.seal graph |> protocol_ok;
                   let writer, _ =
                     Actor.attach subject.child.actor ~mode:Read_write ~subscribe:false
                     |> protocol_ok
                   in
                   protect_cleanup
                     ~finally:(fun () ->
                       read.selected <- None;
                       Actor.detach subject.child.actor writer.id |> protocol_ok)
                     (fun () ->
                        let witnessed = ref None in
                        let metadata_revision =
                          before_remote.identity.metadata_revision
                        in
                        let child_handle = Option.value_exn subject.child.store_handle in
                        read.selected
                        <- Some
                             ( Filename.concat
                                 (Agent_store.Session_store.Handle.directory child_handle)
                                 "ARCHIVED"
                             , fun () ->
                                 assert_sealed owner;
                                 let acknowledgement = native_ack owner in
                                 Host.Native_join.check_acknowledgement
                                   owner
                                   acknowledgement
                                 |> protocol_ok;
                                 witnessed := Some acknowledgement;
                                 let patch =
                                   P.Session_metadata.Patch.create
                                     ~name:(Set "614 actual remote capture changed")
                                     ~set_labels:[]
                                     ~remove_labels:[]
                                   |> protocol_ok
                                 in
                                 within
                                   "actual target metadata ACK after native finish ACK"
                                   (fun () ->
                                      Actor.update_metadata
                                        subject.child.actor
                                        ~attachment_id:writer.id
                                        ~expected_metadata_revision:metadata_revision
                                        ~patch
                                        ()
                                      |> protocol_ok
                                      |> ignore) );
                        let failure =
                          within
                            "native finish ACK then failed remote correction"
                            (fun () ->
                               match Graph.finish graph with
                               | Error error -> error
                               | Ok () ->
                                 failwith "remote capture conflict became finish success")
                        in
                        S.Failure.raise_if_present read.failure;
                        [%test_eq: int] 1 read.hits;
                        assert (Option.is_none read.selected);
                        [%test_eq: P.Error.code] Conflict failure.code;
                        [%test_eq: string]
                          "causal factual Actor/controller/State observation changed"
                          failure.message;
                        let acknowledgement = Option.value_exn !witnessed in
                        assert (phys_equal acknowledgement (native_ack owner));
                        assert_sealed owner;
                        assert (Facts.Host.pending facts |> protocol_ok);
                        assert (Option.is_some (Host.retained_registration owner handle));
                        let failed_remote = state subject.child in
                        [%test_eq: int64]
                          Int64.(metadata_revision + 1L)
                          failed_remote.identity.metadata_revision;
                        assert (
                          Jsonaf.exactly_equal
                            (book_json before_remote)
                            (book_json failed_remote));
                        assert (
                          Jsonaf.exactly_equal
                            (ledger_json corrected)
                            (ledger_json (state subject.parent)));
                        [%test_eq: int] 1 !admissions;
                        [%test_eq: int] 1 !(fixture.direct_providers);
                        within
                          "retry same native ACK and actual remote correction"
                          (fun () -> Graph.finish graph |> protocol_ok);
                        assert_finished owner;
                        assert (phys_equal acknowledgement (native_ack owner));
                        assert (not (Facts.Host.pending facts |> protocol_ok));
                        assert (Option.is_none (Host.retained_registration owner handle));
                        let recovered = state subject.child in
                        assert (
                          Jsonaf.exactly_equal
                            (projection_json expected_projection)
                            (projection_json (Model.pending_projection recovered)));
                        assert (
                          Jsonaf.exactly_equal
                            (ledger_json corrected)
                            (ledger_json (state subject.parent)));
                        let final_book = book_json recovered in
                        within "idempotent exact graph finish" (fun () ->
                          Graph.finish graph |> protocol_ok);
                        assert (phys_equal acknowledgement (native_ack owner));
                        assert (
                          Jsonaf.exactly_equal
                            final_book
                            (book_json (state subject.child)));
                        assert (
                          Jsonaf.exactly_equal
                            (ledger_json corrected)
                            (ledger_json (state subject.parent)));
                        [%test_eq: int] 1 !admissions;
                        [%test_eq: int] 1 !(fixture.direct_providers));
                   Ok receipt)
              |> protocol_ok)
       in
       let output =
         Inference_client.Text.of_receipt receipt
         |> Inference_client.Text.messages
         |> String.concat ~sep:"\n"
       in
       let terminal =
         within "complete actual Model owner" (fun () ->
           Actor.complete_job
             subject.parent.actor
             ~job_id:job.id
             ~generation:job.generation
             ~attempt:job.attempt
             (A.Runtime_builder.Model_succeeded (`String output))
           |> protocol_ok)
       in
       (match terminal.status with
        | Succeeded -> ()
        | _ -> failwith "actual Model owner did not complete");
       assert (P.Job.equal_delivery Not_required terminal.delivery);
       native_terminal (state subject.parent) terminal;
       S.execute_async daemon fixture ~check_completion:(function
         | Succeeded (`String text) ->
           assert (String.is_substring text ~substring:"actual Async native finish")
         | completion -> raise_s [%sexp (completion : P.Completion.t)]);
       let final = S.finish_child env fixture in
       let observed = Model.only "same finished actual Model H" subject.observed in
       S.row final observed.handle |> S.assert_completed;
       [%test_eq: int] 1 !(fixture.direct_providers));
  print_endline
    "actual caused Model H; native join ACK before remote conflict; same graph retry \
     publishes exact newer facts once; native/provider work is not replayed";
  [%expect
    {| actual caused Model H; native join ACK before remote conflict; same graph retry publishes exact newer facts once; native/provider work is not replayed |}]
;;

(* Only the external factual driver inherits this key. Existing Actor/writer
   fibers receive request DATA, never the caller's private dynamic binding. *)
module Lifecycle_read = struct
  module Gate = struct
    type t =
      { entered : unit Eio.Promise.t
      ; enter : unit Eio.Promise.u
      ; released : unit Eio.Promise.t
      ; release : unit Eio.Promise.u
      }

    let create () =
      let entered, enter = Eio.Promise.create () in
      let released, release = Eio.Promise.create () in
      { entered; enter; released; release }
    ;;

    let release t =
      if Option.is_none (Eio.Promise.peek t.released)
      then Eio.Promise.resolve t.release ()
    ;;
  end

  type t =
    { caller : unit Eio.Fiber.key
    ; mutable selected : (string * Gate.t) option
    ; mutable hits : int
    }

  let create () = { caller = Eio.Fiber.create_key (); selected = None; hits = 0 }
  let clear t = t.selected <- None

  let arm t path gate =
    assert (Option.is_none t.selected);
    t.selected <- Some (path, gate)
  ;;

  let with_caller t f = Eio.Fiber.with_binding t.caller () f

  let wrap t env =
    Crash_fault_io.wrap
      env
      ~boundary:Before_stat
      ~matches:(fun path ->
        Option.is_some (Eio.Fiber.get t.caller)
        && Option.exists t.selected ~f:(fun (selected, _) -> String.equal path selected))
      ~reached:(fun _ ->
        let _, gate = Option.value_exn t.selected in
        clear t;
        t.hits <- Int.succ t.hits;
        Eio.Promise.resolve gate.enter ();
        Eio.Promise.await gate.released)
  ;;
end

module Lifecycle_outcome = struct
  let capture f =
    match f () with
    | value -> Ok value
    | exception exn ->
      let backtrace = Stdlib.Printexc.get_raw_backtrace () in
      Error (exn, backtrace)
  ;;

  let resume = function
    | Ok value -> value
    | Error (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
  ;;

  let report (exn, backtrace) =
    try
      Eio.traceln
        "retained lifecycle cleanup failed: %s\n%s"
        (Exn.to_string exn)
        (Stdlib.Printexc.raw_backtrace_to_string backtrace)
    with
    | _ -> ()
  ;;

  let protect ~finally f =
    match capture f with
    | Ok value ->
      Eio.Cancel.protect finally;
      value
    | Error primary ->
      (match capture (fun () -> Eio.Cancel.protect finally) with
       | Ok () -> ()
       | Error cleanup -> report cleanup);
      resume (Error primary)
  ;;
end

let%expect_test "actual retained Model facts survive Hold and failed source close" =
  let module Goal = Goal_service_tests in
  let module G = P.Goal in
  let read = Lifecycle_read.create () in
  let gate_a = Lifecycle_read.Gate.create ()
  and gate_b = Lifecycle_read.Gate.create () in
  let selected_job = ref None in
  let pause_mono = ref ignore
  and resume_mono = ref ignore
  and mono_now = ref (fun () -> failwith "lifecycle scheduler clock absent") in
  S.with_claimed_async
    ~wrap_env:(fun env ->
      let mono_clock, pause, resume, _ = controlled_monotonic_clock env#mono_clock in
      pause_mono := pause;
      resume_mono := resume;
      (mono_now := fun () -> Eio.Time.Mono.now mono_clock);
      Lifecycle_read.wrap
        read
        (object
           method clock = env#clock
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
         : Eio_unix.Stdenv.base))
    ~at_direct:(fun subject ->
      let job = Option.value_exn !selected_job in
      Caused_async_retry_tests.direct_parent subject job;
      S.observe_attempt subject job |> ignore)
    ~scenario:S.Model_finish_cleanup
    ~parent_prompt_text:parent_prompt
    ~sources:
      [ S.original_file, "actual child read"
      ; effect_file, "actual Async native finish"
      ; recipe, "<developer>Actual selected Model source.</developer>"
      ]
    ~model_marker:marker
    ~at_model:(fun _ _ -> failwith "lifecycle fixture executed an unexpected recipe")
    (fun env sw daemon fixture ->
       let subject = fixture.probe in
       let within phase f = Model.within env phase f in
       let born =
         within "lifecycle actual Model admission" (fun () -> model_job fixture)
       in
       let job =
         within "lifecycle actual Model claim" (fun () ->
           Actor.claim_job
             subject.parent.actor
             ~job_id:born.id
             ~generation:born.generation
           |> protocol_ok
           |> Option.value_exn)
       in
       [%test_eq: int] 1 job.attempt;
       assert (P.Job.equal_delivery Not_required job.delivery);
       selected_job := Some job;
       let inference =
         Factory.model_job_inference (Daemon.factory daemon) subject.parent job
         |> protocol_ok
       in
       let target = Inference_runtime.Context.target inference.context in
       within "lifecycle actual recipe target ACK" (fun () ->
         inference.capture_recipe_target target |> protocol_ok);
       assert_target subject job target;
       let request =
         Inference.Request.create
           ~target
           ~history:[]
           ~tools:[]
           ~assets:[]
           ~limits:Transcript.Admission.default
         |> Result.map_error ~f:(fun error ->
           Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))
         |> Result.ok_or_failwith
       in
       let owner = ref None in
       let admissions = ref 0 in
       let queued_usage = ref [] in
       let graph =
         Graph.create
           subject.parent.actor
           ~source:
             (Transcript.Source_id.of_string "614-lifecycle" |> Result.ok_or_failwith)
           ~upstream:
             { new_preparation_id = (fun () -> "614-lifecycle-preparation")
             ; on_admitted = (fun ~scope:_ ~accounting_id:_ -> Int.incr admissions)
             ; on_attempt = ignore
             ; on_observation = ignore
             ; on_completion = ignore
             }
         |> protocol_ok
       in
       let attachment = ref None in
       let graph_task = ref None in
       let finite_close_task = ref None in
       let close_task = ref None in
       Eio.Switch.run (fun task_sw ->
         Lifecycle_outcome.protect
           ~finally:(fun () ->
             (* Every controlled wait is released BEFORE any protected task join. *)
             Lifecycle_read.clear read;
             Lifecycle_read.Gate.release gate_a;
             Lifecycle_read.Gate.release gate_b;
             !resume_mono ();
             fixture.direct := false;
             fixture.release_child ();
             let failures = ref [] in
             let attempt f =
               match Lifecycle_outcome.capture f with
               | Ok () -> ()
               | Error failure -> failures := failure :: !failures
             in
             attempt (fun () ->
               within "lifecycle join actual graph task" (fun () ->
                 Option.iter !graph_task ~f:(fun task ->
                   Eio.Promise.await_exn task |> Lifecycle_outcome.resume |> protocol_ok)));
             attempt (fun () ->
               within "lifecycle join actual finite-close task" (fun () ->
                 Option.iter !finite_close_task ~f:(fun task ->
                   Eio.Promise.await_exn task |> Lifecycle_outcome.resume)));
             attempt (fun () ->
               within "lifecycle join actual close task" (fun () ->
                 Option.iter !close_task ~f:(fun task ->
                   Eio.Promise.await_exn task |> Lifecycle_outcome.resume)));
             attempt (fun () ->
               within "lifecycle final actual graph settlement" (fun () ->
                 Graph.seal graph |> protocol_ok;
                 Graph.finish graph |> protocol_ok));
             attempt (fun () ->
               within "lifecycle final actual source close" (fun () ->
                 Owner.close_and_wait subject.parent.runtime));
             attempt (fun () ->
               within "lifecycle detach actual target writer" (fun () ->
                 Option.iter !attachment ~f:(fun id ->
                   Actor.detach subject.child.actor id |> protocol_ok)));
             match List.rev !failures with
             | [] -> ()
             | first :: rest ->
               List.iter rest ~f:Lifecycle_outcome.report;
               Lifecycle_outcome.resume (Error first))
           (fun () ->
              let receipt =
                Actor.with_job_execution
                  subject.parent.actor
                  ~job_id:job.id
                  ~generation:job.generation
                  ~attempt:job.attempt
                  ~deadline:None
                  (fun execution ->
                     let allocate ~graph ~relation ~configuration =
                       let%map.Result allocation =
                         execution.allocate_inference ~graph ~relation ~configuration
                       in
                       assert (Option.is_none !owner);
                       owner := Some graph;
                       allocation
                     in
                     let identity = Graph.bind_identity graph ~allocate in
                     let receipt =
                       Exn.protect
                         ~finally:(fun () -> fixture.direct := false)
                         ~f:(fun () ->
                           fixture.direct := true;
                           within "lifecycle one actual Model provider" (fun () ->
                             Inference_client.run
                               inference.context
                               ~sw
                               ~identity
                               ~relation:Root
                               ~request
                               ~before_dispatch:ignore
                               ~on_attempt:(Graph.on_attempt graph)
                               ~on_observation:(fun incoming ->
                                 match O.payload incoming with
                                 | Usage _ -> queued_usage := !queued_usage @ [ incoming ]
                                 | Configuration _
                                 | Context_estimate _
                                 | Transport_selection _
                                 | Diagnostic _ -> Graph.on_observation graph incoming)
                               ~on_completion:(Graph.on_completion graph)
                               ~on_event:ignore)
                           |> Result.map_error ~f:(fun error ->
                             Sexp.to_string_hum (Inference_client.Error.sexp_of_t error))
                           |> Result.ok_or_failwith)
                     in
                     Ok receipt)
                |> protocol_ok
              in
              S.Failure.raise_if_present fixture.failure;
              let owner = Option.value_exn !owner in
              let observed = Model.only "lifecycle one actual H" subject.observed in
              let handle = observed.handle in
              let registration =
                Host.retained_registration owner handle |> Option.value_exn
              in
              let facts = actual_facts owner handle in
              let incoming =
                Model.only "lifecycle one actual delayed Usage" !queued_usage
              in
              assert (O.equal incoming (Inference_runtime.Receipt.usage receipt));
              assert (
                Transcript.Scope.equal (Ledger.Handle.scope handle) (O.scope incoming));
              assert (
                O.Observation_id.equal
                  (Ledger.Handle.accounting_id handle)
                  (O.id incoming));
              let output =
                Inference_client.Text.of_receipt receipt
                |> Inference_client.Text.messages
                |> String.concat ~sep:"\n"
              in
              let terminal =
                within "lifecycle complete actual Model Job" (fun () ->
                  Actor.complete_job
                    subject.parent.actor
                    ~job_id:job.id
                    ~generation:job.generation
                    ~attempt:job.attempt
                    (A.Runtime_builder.Model_succeeded (`String output))
                  |> protocol_ok)
              in
              (match terminal.status with
               | Succeeded -> ()
               | Queued
               | Running
               | Waiting_permission _
               | Waiting_completion _
               | Failed _
               | Cancelled
               | Interrupted _ -> failwith "lifecycle actual Model Job did not complete");
              assert (P.Job.equal_delivery Not_required terminal.delivery);
              native_terminal (state subject.parent) terminal;
              S.execute_async daemon fixture ~check_completion:(function
                | Succeeded (`String text) ->
                  assert (String.is_substring text ~substring:"actual Async native finish")
                | completion -> raise_s [%sexp (completion : P.Completion.t)]);
              S.finish_child env fixture |> ignore;
              (* This cut selects Graph.finish as the factual driver after all
                 native work joins. Park both actual maintenance loops before
                 delivering the late Usage; source and phase timeout clocks stay
                 unchanged. Join existing schedule workers before any gate. *)
              fixture.pause ();
              !pause_mono ();
              within "lifecycle actual maintenance schedulers parked" (fun () ->
                let rec await () =
                  S.Failure.raise_if_present fixture.failure;
                  match
                    ( Daemon.For_testing.job_scheduler_wait_deadline daemon
                    , Daemon.For_testing.schedule_scheduler_wait_deadline daemon )
                  with
                  | Some wall, Some mono
                    when Float.(wall > fixture.clock_now ())
                         && Mtime.compare mono (!mono_now ()) > 0 -> ()
                  | Some _, Some _ | Some _, None | None, Some _ | None, None ->
                    Eio.Fiber.yield ();
                    await ()
                in
                await ();
                Daemon.For_testing.await_schedule_scheduler_idle daemon);
              let quiet entry =
                within "lifecycle actual Actor execution quiescence" (fun () ->
                  let rec await () =
                    match
                      Actor.with_quiescent_state entry.Registry.actor ~f:(fun current ->
                        Ok current)
                      |> protocol_ok
                    with
                    | Some _ -> ()
                    | None ->
                      Eio.Fiber.yield ();
                      await ()
                  in
                  await ())
              in
              quiet subject.parent;
              quiet subject.child;
              within "lifecycle actual unrelated source cleanup" (fun () ->
                Actor.drain_callback_retirement subject.parent.actor |> protocol_ok;
                Actor.drain_constructor_model_retirement subject.parent.actor
                |> protocol_ok;
                Actor.drain_descendant_cleanup subject.parent.actor |> protocol_ok;
                Actor.drain_inference_usage subject.parent.actor |> protocol_ok);
              let no_unfinished_source current =
                (* These prompts configure no Run action or recurrence producer.
                   A live Workflow label alone does not mean owned work remains. *)
                assert (List.is_empty current.A.Session_state.schedules);
                assert (List.is_empty current.subscriptions);
                Option.iter current.recurrence_catalog ~f:(fun catalog ->
                  assert (List.is_empty (A.Recurrence_catalog.entries catalog)));
                Option.iter current.run_state ~f:(fun runs ->
                  assert (List.is_empty (A.Run_state.recurrence_origins runs));
                  List.iter (A.Run_state.intents runs) ~f:(fun intent ->
                    match intent.A.Run_intent.disposition with
                    | Pending ->
                      failwith "lifecycle guard would retire a pending Run intent"
                    | Consumed _ | Retired -> ());
                  List.iter (A.Run_state.runs runs) ~f:(fun (run : P.Run.t) ->
                    List.iter run.owned_work ~f:(fun work ->
                      assert (
                        List.mem run.relinquished_work work ~equal:P.Run_work.equal
                        || List.exists run.terminal_work ~f:(fun proof ->
                          P.Run_work.equal proof.P.Run_work.Terminal.work work)))))
              in
              let clean_guard entry =
                within "lifecycle actual no-descendant admission guard" (fun () ->
                  Eio.Cancel.protect (fun () ->
                    let current = state entry in
                    no_unfinished_source current;
                    let fence =
                      Actor.begin_lifecycle
                        entry.Registry.actor
                        ~attachment_id:None
                        ~expected_generation:current.identity.generation
                        ~expected_revision:current.counters.revision
                      |> protocol_ok
                    in
                    (* begin_lifecycle closes process-local Run/recurrence owners.
                       The actual public work checks above precede that mutation;
                       abort before ANY Runtime close or irreversible effect. *)
                    Actor.abort_lifecycle entry.actor fence |> protocol_ok;
                    assert (
                      Option.equal
                        A.Run_state.equal
                        current.run_state
                        (state entry).run_state)))
              in
              clean_guard subject.parent;
              clean_guard subject.child;
              let retained entry =
                let current =
                  Registry.find (Daemon.registry daemon) (state entry).identity.session_id
                  |> Option.value_exn
                in
                assert (phys_equal current.actor entry.Registry.actor);
                assert (phys_equal current.runtime entry.runtime);
                let handle = Option.value_exn entry.store_handle in
                assert (Option.exists current.store_handle ~f:(phys_equal handle));
                Agent_store.Session_store.Handle.metadata_checked handle
                |> Result.map_error ~f:(fun error ->
                  Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t error))
                |> Result.ok_or_failwith
                |> ignore
              in
              let before_target_close = state subject.child in
              within "lifecycle actual target Runtime preclose" (fun () ->
                Owner.close_and_wait subject.child.runtime;
                Owner.close_and_wait subject.child.runtime);
              retained subject.parent;
              retained subject.child;
              let target_closed = state subject.child in
              assert (
                Jsonaf.exactly_equal
                  (book_json before_target_close)
                  (book_json target_closed));
              assert (
                Jsonaf.exactly_equal
                  (ledger_json before_target_close)
                  (ledger_json target_closed));
              assert (
                Jsonaf.exactly_equal
                  (G.to_json (Goal.goal before_target_close))
                  (G.to_json (Goal.goal target_closed)));
              let stable_target_ledger = ledger_json target_closed in
              let assert_target_ledger () =
                assert (
                  Jsonaf.exactly_equal
                    stable_target_ledger
                    (ledger_json (state subject.child)))
              in
              let loaded = ref false in
              within "lifecycle source is genuinely live before close" (fun () ->
                Owner.with_loaded_runtime subject.parent.runtime (fun _ ->
                  loaded := true;
                  Ok ())
                |> protocol_ok);
              assert !loaded;
              (match Host.phase owner with
               | Open -> ()
               | Sealed | Finished -> failwith "native completion lost actual open graph");
              assert (Option.is_none (Host.Native_join.current owner |> protocol_ok));
              assert (
                phys_equal
                  registration
                  (Host.retained_registration owner handle |> Option.value_exn));
              assert (phys_equal facts (actual_facts owner handle));
              assert (Facts.Host.initial_confirmed facts |> protocol_ok);
              assert (not (Facts.Host.pending facts |> protocol_ok));
              let before_late = state subject.parent in
              let expected_parent_attempts =
                Int64.(Model.committed subject.before_parent + 2L)
              in
              [%test_eq: int64] expected_parent_attempts (Model.committed before_late);
              [%test_eq: int] 1 !admissions;
              [%test_eq: int] 1 !(fixture.direct_providers);
              [%test_eq: int] 1 !(fixture.child_providers);
              [%test_eq: int]
                1
                (List.length (Model.new_rows subject.before_parent before_late));
              let row = S.row before_late handle in
              S.assert_completed row;
              assert (
                not
                  (List.exists
                     (O.Attempt_record.observations (Ledger.Row.record row))
                     ~f:(fun observation ->
                       O.Key.equal (O.key observation) (O.key incoming))));
              let old_projection = Model.pending_projection (state subject.child) in
              within "lifecycle actual late Usage ACK" (fun () ->
                Actor.observe_owned_inference subject.parent.actor ~owner incoming
                |> protocol_ok);
              let corrected = state subject.parent in
              [%test_eq: int64]
                Int64.(Ledger.revision before_late.inference_ledger + 1L)
                (Ledger.revision corrected.inference_ledger);
              [%test_eq: int64]
                Int64.(before_late.counters.revision + 1L)
                corrected.counters.revision;
              let retained_usage =
                S.row corrected handle
                |> Ledger.Row.record
                |> O.Attempt_record.observations
                |> List.filter ~f:(fun observation ->
                  O.Key.equal (O.key observation) (O.key incoming))
                |> Model.only "lifecycle actual Usage retained after ACK"
              in
              assert (O.equal incoming retained_usage);
              let expected_projection =
                PC.projections
                  (Option.value_exn corrected.policy_canonical_usage)
                  ~ledger:corrected.inference_ledger
                  ~session_revision:corrected.counters.revision
                |> protocol_ok
                |> List.filter ~f:(fun projection ->
                  PC.Stream.equal observed.stream (PC.Projection.stream projection))
                |> Model.only "lifecycle exact late projection"
              in
              assert (
                Int64.(
                  PC.Projection.session_revision expected_projection
                  > PC.Projection.session_revision old_projection));
              [%test_eq: int64]
                (PC.Projection.admitted_attempts old_projection)
                (PC.Projection.admitted_attempts expected_projection);
              assert (Facts.Host.pending facts |> protocol_ok);
              Graph.seal graph |> protocol_ok;
              let writer, _ =
                Actor.attach subject.child.actor ~mode:Read_write ~subscribe:false
                |> protocol_ok
              in
              attachment := Some writer.id;
              let target_path =
                Filename.concat
                  (Agent_store.Session_store.Handle.directory
                     (Option.value_exn subject.child.store_handle))
                  "ARCHIVED"
              in
              let await_gate phase gate task ~ended =
                within phase (fun () ->
                  Eio.Fiber.first
                    (fun () -> Eio.Promise.await gate.Lifecycle_read.Gate.entered)
                    (fun () ->
                       Eio.Promise.await_exn task |> Lifecycle_outcome.resume |> ended))
              in
              let assert_closed () =
                let entered = ref false in
                let failure =
                  Owner.with_loaded_runtime subject.parent.runtime (fun _ ->
                    entered := true;
                    Ok ())
                  |> rejected
                in
                assert (not !entered);
                [%test_eq: P.Error.code] Server_shutting_down failure.code;
                [%test_eq: string] "session runtime owner is closed" failure.message;
                [%test_eq: bool] true failure.retryable
              in
              let jobs_json current =
                `Array (List.map current.A.Session_state.jobs ~f:P.Job.to_json)
              in
              let stable_jobs = jobs_json corrected in
              let assert_native_unchanged () =
                assert_target_ledger ();
                let current = state subject.parent in
                assert (Jsonaf.exactly_equal (ledger_json corrected) (ledger_json current));
                assert (Jsonaf.exactly_equal stable_jobs (jobs_json current));
                [%test_eq: int]
                  1
                  (List.length (Model.new_rows subject.before_parent current));
                [%test_eq: int] 1 !admissions;
                [%test_eq: int] 1 !(fixture.direct_providers);
                [%test_eq: int] 1 !(fixture.child_providers);
                S.Failure.raise_if_present fixture.failure
              in
              let assert_conflict error =
                [%test_eq: P.Error.code] Conflict error.P.Error.code;
                [%test_eq: string]
                  "causal factual Actor/controller/State observation changed"
                  error.message;
                [%test_eq: bool] false error.retryable
              in
              Lifecycle_read.arm read target_path gate_a;
              let finishing =
                Eio.Fiber.fork_promise ~sw:task_sw (fun () ->
                  Lifecycle_outcome.capture (fun () ->
                    Lifecycle_read.with_caller read (fun () -> Graph.finish graph)))
              in
              graph_task := Some finishing;
              await_gate
                "lifecycle A actual running remote correction"
                gate_a
                finishing
                ~ended:(function
                | Error error ->
                  raise_s
                    [%sexp
                      "actual graph failed before lifecycle cut", (error : P.Error.t)]
                | Ok () -> failwith "actual graph finished before lifecycle cut");
              assert_sealed owner;
              let acknowledgement = native_ack owner in
              Host.Native_join.check_acknowledgement owner acknowledgement |> protocol_ok;
              assert (Facts.Host.pending facts |> protocol_ok);
              let before_hold = state subject.child in
              let previous_goal = Goal.goal before_hold in
              within "lifecycle A actual explicit target Hold" (fun () ->
                Goal_admitted_guard_tests.command
                  daemon
                  subject.child
                  ~key:"614-lifecycle-actual-hold"
                  (Hold (Goal.goal_id, Operator_pause))
                |> protocol_ok
                |> ignore);
              let held_state = state subject.child in
              let held_goal = Goal.goal held_state in
              assert (P.Session_ref.equal (G.session previous_goal) (G.session held_goal));
              assert (P.Id.Goal.equal (G.id previous_goal) (G.id held_goal));
              assert (P.Run_source.equal (G.source previous_goal) (G.source held_goal));
              assert (
                Option.equal
                  G.Episode.equal
                  (G.episode previous_goal)
                  (G.episode held_goal));
              [%test_eq: int64]
                Int64.(G.revision previous_goal + 1L)
                (G.revision held_goal);
              assert (G.State.equal (Open (Held Operator_pause)) (G.state held_goal));
              assert (Int64.(held_state.counters.revision > before_hold.counters.revision));
              let finite_closing =
                Eio.Fiber.fork_promise ~sw:task_sw (fun () ->
                  Lifecycle_outcome.capture (fun () -> Owner.close subject.parent.runtime))
              in
              finite_close_task := Some finite_closing;
              within "lifecycle A finite source close while remote waits" (fun () ->
                let outcome = Eio.Promise.await_exn finite_closing in
                finite_close_task := None;
                Lifecycle_outcome.resume outcome);
              assert_closed ();
              assert (phys_equal acknowledgement (native_ack owner));
              assert (Facts.Host.pending facts |> protocol_ok);
              Lifecycle_read.Gate.release gate_a;
              let failure =
                within "lifecycle A exact original correction failure" (fun () ->
                  let outcome = Eio.Promise.await_exn finishing in
                  graph_task := None;
                  Lifecycle_outcome.resume outcome |> rejected)
              in
              assert_conflict failure;
              let assert_retained () =
                assert_sealed owner;
                assert (phys_equal acknowledgement (native_ack owner));
                assert (
                  phys_equal
                    registration
                    (Host.retained_registration owner handle |> Option.value_exn));
                assert (phys_equal facts (actual_facts owner handle));
                assert (Facts.Host.initial_confirmed facts |> protocol_ok);
                assert (Facts.Host.pending facts |> protocol_ok);
                assert_closed ();
                assert_native_unchanged ()
              in
              assert_retained ();
              assert (
                Jsonaf.exactly_equal
                  (book_json held_state)
                  (book_json (state subject.child)));
              Lifecycle_read.arm read target_path gate_b;
              let closing =
                Eio.Fiber.fork_promise ~sw:task_sw (fun () ->
                  Lifecycle_outcome.capture (fun () ->
                    Lifecycle_read.with_caller read (fun () ->
                      Owner.close_and_wait subject.parent.runtime)))
              in
              close_task := Some closing;
              await_gate
                "lifecycle B close leads actual remote correction"
                gate_b
                closing
                ~ended:(fun () ->
                  failwith "actual source close finished before lifecycle cut");
              assert_retained ();
              retained subject.parent;
              retained subject.child;
              let before_metadata = state subject.child in
              let patch =
                P.Session_metadata.Patch.create
                  ~name:(Set "614 lifecycle retained close capture changed")
                  ~set_labels:[]
                  ~remove_labels:[]
                |> protocol_ok
              in
              within "lifecycle B actual target metadata ACK" (fun () ->
                Actor.update_metadata
                  subject.child.actor
                  ~attachment_id:writer.id
                  ~expected_metadata_revision:before_metadata.identity.metadata_revision
                  ~patch
                  ()
                |> protocol_ok
                |> ignore);
              let after_metadata = state subject.child in
              [%test_eq: int64]
                Int64.(before_metadata.identity.metadata_revision + 1L)
                after_metadata.identity.metadata_revision;
              Lifecycle_read.Gate.release gate_b;
              let close_outcome =
                within "lifecycle B finite exact strong-close failure" (fun () ->
                  let outcome = Eio.Promise.await_exn closing in
                  close_task := None;
                  outcome)
              in
              (match close_outcome with
               | Error (Owner.Cleanup_failed error, _) -> assert_conflict error
               | Error failure -> Lifecycle_outcome.resume (Error failure)
               | Ok () -> failwith "changed remote capture became source-close success");
              assert_retained ();
              assert (
                Jsonaf.exactly_equal
                  (book_json after_metadata)
                  (book_json (state subject.child)));
              [%test_eq: int] 2 read.hits;
              assert (Option.is_none read.selected);
              within "lifecycle same factual graph retries after retirement" (fun () ->
                Graph.finish graph |> protocol_ok);
              assert_finished owner;
              assert (phys_equal acknowledgement (native_ack owner));
              assert (not (Facts.Host.pending facts |> protocol_ok));
              assert (Option.is_none (Host.retained_registration owner handle));
              let recovered = state subject.child in
              assert (
                Jsonaf.exactly_equal
                  (projection_json expected_projection)
                  (projection_json (Model.pending_projection recovered)));
              assert (
                G.State.equal (Open (Held Operator_pause)) (G.state (Goal.goal recovered)));
              let assert_totals entry expected =
                let current = state entry in
                [%test_eq: int64] expected (Model.committed current);
                [%test_eq: int64]
                  0L
                  (S.Totals.View.pending (Model.total current) ~dimension:Attempt)
              in
              assert_totals subject.parent expected_parent_attempts;
              assert_totals subject.child subject.ceiling;
              let final_book = book_json recovered in
              within "lifecycle idempotent exact graph finish" (fun () ->
                Graph.finish graph |> protocol_ok);
              within "lifecycle explicit successful source-close retry" (fun () ->
                Owner.close_and_wait subject.parent.runtime;
                Owner.close_and_wait subject.parent.runtime);
              assert_closed ();
              retained subject.parent;
              retained subject.child;
              assert_native_unchanged ();
              assert (phys_equal acknowledgement (native_ack owner));
              assert (Jsonaf.exactly_equal final_book (book_json (state subject.child))))));
  print_endline
    "same actual Model facts survive Hold and source retirement during remote wait; \
     failed strong close retains exact ACK for factual retry without replay";
  [%expect
    {| same actual Model facts survive Hold and source retirement during remote wait; failed strong close retains exact ACK for factual retry without replay |}]
;;
