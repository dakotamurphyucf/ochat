open! Core
open Agent_server_test_support
open Retry_support
module S = Caused_async_support
module Model = S.Model
module PC = S.PC
module Cause = S.Cause
module Book = S.Book
module Ledger = S.Ledger
module Graph = S.Graph
module Runtime = Inference_runtime
module Observation = Inference.Observation

(* These are observations of the actual public Book carrier, never receipts used
   to authorize a writer or construct a factual registration. *)
type slot =
  { pending : Book.Pending.t option
  ; projection : PC.Projection.t
  }

let slot current stream =
  let payload =
    Option.value_exn current.A.Session_state.goal_policy_contributions
    |> Book.to_document
    |> protocol_ok
    |> Document_schema.Document.payload
  in
  let book =
    Jsonaf.member_exn "books" payload
    |> Jsonaf.list_exn
    |> List.filter ~f:(fun book ->
      let id = Jsonaf.member_exn "goal_id" book |> P.Id.Goal.of_json |> protocol_ok in
      P.Id.Goal.equal id S.Goal.goal_id)
    |> Model.only "actual initial-first Goal book"
  in
  let raw =
    Jsonaf.member_exn "slots" book
    |> Jsonaf.list_exn
    |> List.filter ~f:(fun slot ->
      let actual = Jsonaf.member_exn "stream" slot |> PC.Stream.of_json |> protocol_ok in
      PC.Stream.equal stream actual)
    |> Model.only "actual initial-first remote stream"
  in
  let pending =
    match Jsonaf.member_exn "pending" raw with
    | `Null -> None
    | raw -> Some (Book.Pending.of_json raw |> protocol_ok)
  in
  let projection =
    Jsonaf.member_exn "projection" raw |> PC.Projection.of_json |> protocol_ok
  in
  let actual_pending = Book.Subtotal.pending (Model.subtotal current) in
  assert (
    List.equal
      A.Goal_descendant_occurrence.equal
      actual_pending
      (Option.to_list pending |> List.map ~f:Book.Pending.occurrence));
  { pending; projection }
;;

let projection current stream =
  PC.projections
    (Option.value_exn current.A.Session_state.policy_canonical_usage)
    ~ledger:current.inference_ledger
    ~session_revision:current.counters.revision
  |> protocol_ok
  |> List.filter ~f:(fun projection ->
    PC.Stream.equal stream (PC.Projection.stream projection))
  |> Model.only "actual source initial-first projection"
;;

let same_projection before after =
  assert (
    Jsonaf.exactly_equal (PC.Projection.to_json before) (PC.Projection.to_json after))
;;

let book_json current =
  Option.value_exn current.A.Session_state.goal_policy_contributions
  |> Book.to_document
  |> protocol_ok
  |> Document_schema.Document.json
;;

let ledger_json current =
  Ledger.to_document current.A.Session_state.inference_ledger
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (Ledger.Error.sexp_of_t error))
  |> Result.ok_or_failwith
  |> Document_schema.Document.json
;;

let assert_attempts (subject : S.probe) ~source_delta ~remote_delta ~pending =
  let parent = state subject.parent
  and child = state subject.child in
  let source_committed = Model.committed parent
  and remote_committed = Model.committed child in
  let source_pending = S.Totals.View.pending (Model.total parent) ~dimension:Attempt
  and remote_pending = S.Totals.View.pending (Model.total child) ~dimension:Attempt in
  [%test_eq: int64]
    Int64.(Model.committed subject.before_parent + source_delta)
    source_committed;
  [%test_eq: int64]
    Int64.(Model.committed subject.before_child + remote_delta)
    remote_committed;
  [%test_eq: int64] 0L source_pending;
  [%test_eq: int64] pending remote_pending;
  [%test_eq: int64]
    Int64.(Model.committed subject.before_child + remote_delta + pending)
    Int64.(remote_committed + remote_pending)
;;

let assert_conflict error =
  [%test_eq: P.Error.code] Conflict error.P.Error.code;
  [%test_eq: string]
    "causal factual Actor/controller/State observation changed"
    error.message;
  [%test_eq: bool] false error.retryable
;;

let rejected f =
  match f () with
  | Error error -> assert_conflict error
  | Ok _ -> failwith "initial-first fault unexpectedly admitted fresh inference"
  | exception Graph.Rejected error -> assert_conflict error
;;

(* Cleanup keeps every actual owner live until all joins are attempted. A body
   failure remains primary; an independent cleanup failure is retained in output. *)
let report_cleanup (exn, backtrace) =
  try
    Eio.traceln
      "initial-first cleanup failed: %s\n%s"
      (Exn.to_string exn)
      (Stdlib.Printexc.raw_backtrace_to_string backtrace)
  with
  | _ -> ()
;;

let protect_owned ~finally f =
  match f () with
  | value ->
    Eio.Cancel.protect finally;
    value
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    (match Eio.Cancel.protect finally with
     | () -> ()
     | exception cleanup ->
       let cleanup_backtrace = Stdlib.Printexc.get_raw_backtrace () in
       report_cleanup (cleanup, cleanup_backtrace));
    Exn.raise_with_original_backtrace exn backtrace
;;

module Read = struct
  type t =
    { mutable path : string option
    ; mutable run : (unit -> unit) option
    ; mutable active : bool
    ; mutable hits : int
    ; mutable excluded : int
    ; caller : unit Eio.Fiber.key
    ; failure : S.Failure.t
    }

  let create () =
    { path = None
    ; run = None
    ; active = false
    ; hits = 0
    ; excluded = 0
    ; caller = Eio.Fiber.create_key ()
    ; failure = S.Failure.create ()
    }
  ;;

  let clear t = t.run <- None
  let arm t run = t.run <- Some run

  (* Actors and writers already exist before this caller-only binding. Actor.call
     queues request DATA; it never transfers this private fiber context. *)
  let with_caller t f = Eio.Fiber.with_binding t.caller () f

  let wrap t env =
    Crash_fault_io.wrap
      env
      ~boundary:Before_stat
      ~matches:(fun path ->
        if
          t.active
          || Option.is_none t.run
          || not (Option.exists t.path ~f:(String.equal path))
        then false
        else (
          (* Metadata publication reads ARCHIVED inside the Actor mailbox too.
             Exclude those reads BEFORE any callback or Actor RPC. *)
          match Eio.Fiber.get t.caller with
          | Some () -> true
          | None ->
            t.excluded <- Int.min 129 (Int.succ t.excluded);
            false))
      ~reached:(fun _ ->
        let run = Option.value_exn t.run in
        t.active <- true;
        Exn.protect
          ~finally:(fun () -> t.active <- false)
          ~f:(fun () ->
            S.Failure.guard t.failure (fun () ->
              t.hits <- Int.succ t.hits;
              assert (t.hits <= 128);
              run ())))
  ;;
end

let metadata_ack subject attachment =
  let before = state subject.S.child in
  let name =
    "614 real initial-first capture " ^ Int64.to_string before.identity.metadata_revision
  in
  let patch =
    P.Session_metadata.Patch.create ~name:(Set name) ~set_labels:[] ~remove_labels:[]
    |> protocol_ok
  in
  Actor.update_metadata
    subject.child.actor
    ~attachment_id:attachment
    ~expected_metadata_revision:before.identity.metadata_revision
    ~patch
    ()
  |> protocol_ok
  |> ignore;
  let after = state subject.child in
  [%test_eq: int64]
    Int64.(before.identity.metadata_revision + 1L)
    after.identity.metadata_revision;
  [%test_eq: string option] (Some name) after.identity.display_name
;;

let assert_binding (subject : S.probe) (first : S.observation) current handle =
  let accounting = Option.value_exn current.A.Session_state.policy_canonical_usage in
  let binding =
    PC.bindings accounting
    |> List.filter ~f:(fun binding ->
      Int64.equal (PC.Binding.ordinal binding) (Ledger.Handle.ordinal handle))
    |> Model.only "actual initial-first H binding"
  in
  let cause = Model.cause_for accounting binding in
  assert (Cause.equal first.cause cause);
  let job =
    List.find_exn current.jobs ~f:(fun job -> P.Id.Job.equal job.id subject.job.id)
  in
  [%test_eq: int] subject.job.attempt job.attempt;
  [%test_eq: P.Job.kind] Async_tool job.kind;
  (match job.status with
   | Running -> ()
   | Queued
   | Waiting_permission _
   | Waiting_completion _
   | Succeeded
   | Failed _
   | Cancelled
   | Interrupted _ -> failwith "initial-first actual Async owner ended");
  assert (
    Cause.equal
      cause
      (S.source_cause current (PC.Source.Native_prefix.member_of_job job |> protocol_ok)))
;;

let assert_pending (subject : S.probe) (first : S.observation) handle pending =
  assert (
    P.Session_ref.equal
      (Book.Pending.owner pending)
      (S.G.session (S.Goal.goal (state subject.child))));
  assert (P.Id.Goal.equal (Book.Pending.goal_id pending) S.Goal.goal_id);
  [%test_eq: string] (PC.Stream.id first.stream) (Book.Pending.stream_id pending);
  assert (Cause.Key.equal (Cause.key first.cause) (Book.Pending.cause pending));
  [%test_eq: int64] 2L (Book.Pending.next_prefix pending);
  (match A.Goal_descendant_occurrence.kind (Book.Pending.occurrence pending) with
   | Attempt { ordinal; _ } -> [%test_eq: int64] (Ledger.Handle.ordinal handle) ordinal
   | Host_turn _ -> failwith "H2 initial Pending lost its native attempt");
  match Book.Pending.work pending with
  | Async_job_attempt { id; attempt } ->
    assert (P.Id.Job.equal subject.job.id id);
    [%test_eq: int] subject.job.attempt attempt
  | _ -> failwith "H2 initial Pending changed its actual Async work"
;;

let%expect_test "actual H1 correction waits for H2 initial before fresh graph admission" =
  let read = Read.create () in
  let first = ref None in
  let third_scope = ref None in
  let direct_calls = ref 0 in
  let third_provider = ref false in
  let pause_mono = ref ignore
  and resume_mono = ref ignore
  and mono_now = ref (fun () -> failwith "initial-first scheduler clock absent") in
  let at_direct (subject : S.probe) =
    Int.incr direct_calls;
    match !direct_calls with
    | 1 -> first := Some (S.observe_attempt subject subject.job)
    | 2 ->
      let first = Option.value_exn !first in
      let current = state subject.parent in
      [%test_eq: int] 3 (List.length (Model.new_rows subject.before_parent current));
      let scope = Option.value_exn !third_scope in
      let row =
        Model.new_rows subject.before_parent current
        |> List.filter ~f:(fun row ->
          Transcript.Scope.equal scope (Ledger.Handle.scope (Ledger.Row.handle row)))
        |> Model.only "actual H3 at its sole provider"
      in
      (match Observation.Attempt_record.state (Ledger.Row.record row) with
       | Running -> ()
       | Prepared | Terminal _ | Interrupted _ ->
         failwith "H3 provider preceded native ACK");
      assert_binding subject first current (Ledger.Row.handle row);
      let imported = slot (state subject.child) first.stream in
      assert (Option.is_none imported.pending);
      [%test_eq: int64] 3L (PC.Projection.admitted_attempts imported.projection);
      assert_attempts subject ~source_delta:4L ~remote_delta:4L ~pending:0L;
      third_provider := true
    | _ -> failwith "initial-first trajectory replayed a provider"
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
    ~scenario:S.Initial_first_drain
    ~parent_prompt_text:Caused_async_retry_tests.parent_prompt
    ~sources:
      [ S.original_file, "real initial-first child read"
      ; Caused_async_retry_tests.effect_file, "real initial-first Async worker"
      ; ( Caused_async_retry_tests.recipe
        , "<developer>unused initial-first recipe.</developer>" )
      ]
    ~model_marker:"614_INITIAL_FIRST_UNEXPECTED_MODEL"
    ~at_model:(fun _ _ -> failwith "initial-first test admitted an unexpected Model Job")
    (fun env sw daemon fixture ->
       let subject = fixture.probe in
       let path =
         Filename.concat
           (Agent_store.Session_store.Handle.directory
              (Option.value_exn subject.child.store_handle))
           "ARCHIVED"
       in
       read.path <- Some path;
       let attachment, _ =
         Actor.attach subject.child.actor ~mode:Read_write ~subscribe:false |> protocol_ok
       in
       protect_owned
         ~finally:(fun () ->
           !resume_mono ();
           Read.clear read;
           fixture.direct := false;
           Model.within env "detach actual initial-first metadata writer" (fun () ->
             Actor.detach subject.child.actor attachment.id |> protocol_ok))
         (fun () ->
            (* Select the caller-owned initial/correction driver. Wall polling is
               already parked by S; stop the independent Mono idle sweeper and
               join its actual workers before creating any new factual work. *)
            !pause_mono ();
            Model.within
              env
              "initial-first actual maintenance schedulers parked"
              (fun () ->
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
            let runtime =
              Owner.with_loaded_runtime subject.parent.runtime (fun runtime -> Ok runtime)
              |> protocol_ok
            in
            let context, request = S.Manual.prepared runtime in
            Actor.with_job_execution
              subject.parent.actor
              ~job_id:subject.job.id
              ~generation:subject.job.generation
              ~attempt:subject.job.attempt
              ~deadline:None
              (fun execution ->
                 Read.with_caller read
                 @@ fun () ->
                 let graphs = ref [] in
                 let second_owner = ref None in
                 let new_graph label ~on_admitted =
                   let graph =
                     Graph.create
                       subject.parent.actor
                       ~source:
                         (Transcript.Source_id.of_string label |> Result.ok_or_failwith)
                       ~upstream:
                         { new_preparation_id = (fun () -> label ^ "-preparation")
                         ; on_admitted
                         ; on_attempt = ignore
                         ; on_observation = ignore
                         ; on_completion = ignore
                         }
                     |> protocol_ok
                   in
                   graphs := graph :: !graphs;
                   graph
                 in
                 let run graph ~on_observation =
                   fixture.direct := true;
                   Exn.protect
                     ~finally:(fun () -> fixture.direct := false)
                     ~f:(fun () ->
                       Inference_client.run
                         context
                         ~sw
                         ~identity:
                           (Graph.bind_identity
                              graph
                              ~allocate:execution.allocate_inference)
                         ~relation:Root
                         ~request
                         ~before_dispatch:ignore
                         ~on_attempt:(Graph.on_attempt graph)
                         ~on_observation
                         ~on_completion:(Graph.on_completion graph)
                         ~on_event:ignore
                       |> Result.map_error ~f:(fun error ->
                         P.Error.invalid_request
                           (Sexp.to_string_hum (Inference_client.Error.sexp_of_t error))))
                 in
                 protect_owned
                   ~finally:(fun () ->
                     (* Resume before any protected Graph/native/child joins. *)
                     !resume_mono ();
                     Read.clear read;
                     let failures = ref [] in
                     let attempt f =
                       match f () with
                       | () -> ()
                       | exception exn ->
                         let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                         failures := (exn, backtrace) :: !failures
                     in
                     List.iter !graphs ~f:(fun graph ->
                       attempt (fun () ->
                         Model.within
                           env
                           "join actual initial-first Graph owner"
                           (fun () ->
                              Graph.seal graph |> protocol_ok;
                              Graph.finish graph |> protocol_ok)));
                     Option.iter !second_owner ~f:(fun owner ->
                       attempt (fun () ->
                         Model.within env "join actual retained H2 owner" (fun () ->
                           Actor.seal_inference_owner subject.parent.actor ~owner
                           |> protocol_ok;
                           Actor.finish_inference_owner subject.parent.actor ~owner
                           |> protocol_ok)));
                     match List.rev !failures with
                     | [] -> ()
                     | (failure, backtrace) :: rest ->
                       List.iter rest ~f:report_cleanup;
                       Exn.raise_with_original_backtrace failure backtrace)
                   (fun () ->
                      let queued = ref [] in
                      let h1 =
                        new_graph
                          "614-initial-first-h1"
                          ~on_admitted:(fun ~scope:_ ~accounting_id:_ -> ())
                      in
                      let first_receipt =
                        run h1 ~on_observation:(fun incoming ->
                          match Observation.payload incoming with
                          | Usage _ -> queued := incoming :: !queued
                          | Configuration _
                          | Context_estimate _
                          | Transport_selection _
                          | Diagnostic _ -> Graph.on_observation h1 incoming)
                        |> protocol_ok
                      in
                      [%test_eq: int] 1 !(fixture.direct_providers);
                      let first = Option.value_exn !first in
                      let usage =
                        Model.only "actual delayed H1 provider Usage" (List.rev !queued)
                      in
                      assert (
                        Observation.equal usage (Runtime.Receipt.usage first_receipt));
                      assert (
                        Transcript.Scope.equal
                          (Ledger.Handle.scope first.handle)
                          (Observation.scope usage));
                      (match Observation.payload usage with
                       | Usage _ -> ()
                       | Context_estimate _
                       | Configuration _
                       | Transport_selection _
                       | Diagnostic _ ->
                         failwith "actual delayed H1 callback was not Usage");
                      S.assert_completed (S.row (state subject.parent) first.handle);
                      assert (
                        not (Actor.inference_usage_cleanup_pending subject.parent.actor));
                      assert_attempts
                        subject
                        ~source_delta:2L
                        ~remote_delta:2L
                        ~pending:0L;
                      let before_h2 = state subject.parent in
                      let h2 = ref None in
                      let pending = ref None in
                      let initial_projection = ref None in
                      let h2_initial_faults = ref 0 in
                      let pre_canonical_reads = ref 0 in
                      let h2_state () =
                        let source = state subject.parent in
                        let rows = Model.new_rows before_h2 source in
                        match rows with
                        | [] -> None
                        | [ row ] ->
                          let handle = Ledger.Row.handle row in
                          Option.iter !h2 ~f:(fun previous ->
                            assert (Ledger.Handle.equal previous handle));
                          assert_binding subject first source handle;
                          let current = slot (state subject.child) first.stream in
                          let actual_pending = Option.value_exn current.pending in
                          assert_pending subject first handle actual_pending;
                          Option.iter !pending ~f:(fun previous ->
                            assert (Book.Pending.equal previous actual_pending));
                          Some (source, handle, current, actual_pending)
                        | _ -> failwith "H2 acquired more than one actual native row"
                      in
                      let fault_initial ?after () =
                        Read.arm read (fun () ->
                          match h2_state () with
                          | None -> Int.incr pre_canonical_reads
                          | Some (source, handle, current, actual_pending) ->
                            Read.clear read;
                            [%test_eq: int64]
                              1L
                              (PC.Projection.admitted_attempts current.projection);
                            h2 := Some handle;
                            pending := Some actual_pending;
                            if Option.is_none !initial_projection
                            then
                              initial_projection := Some (projection source first.stream);
                            Int.incr h2_initial_faults;
                            metadata_ack subject attachment.id;
                            Option.iter after ~f:(fun next -> next ()))
                      in
                      fault_initial ();
                      let owner =
                        Eio.Cancel.protect (fun () ->
                          Actor.open_inference_owner
                            subject.parent.actor
                            ~source:
                              (Transcript.Source_id.of_string "614-initial-first-h2"
                               |> Result.ok_or_failwith))
                        |> protocol_ok
                      in
                      second_owner := Some owner;
                      let prepared =
                        Runtime.Context.prepare
                          context
                          ~preparation_id:"614-initial-first-h2-preparation"
                          request
                        |> Result.map_error ~f:(fun error ->
                          Sexp.to_string_hum (Runtime.Preparation_error.sexp_of_t error))
                        |> Result.ok_or_failwith
                      in
                      Model.within
                        env
                        "actual H2 canonical ACK and initial conflict"
                        (fun () ->
                           rejected (fun () ->
                             execution.allocate_inference
                               ~graph:owner
                               ~relation:Root
                               ~configuration:(Runtime.Prepared.configuration prepared)));
                      S.Failure.raise_if_present read.failure;
                      [%test_eq: int] 1 !h2_initial_faults;
                      let h2 = Option.value_exn !h2 in
                      let pending = Option.value_exn !pending in
                      let initial_projection = Option.value_exn !initial_projection in
                      [%test_eq: int64]
                        2L
                        (PC.Projection.admitted_attempts initial_projection);
                      (match
                         Observation.Attempt_record.state
                           (Ledger.Row.record (S.row (state subject.parent) h2))
                       with
                       | Interrupted { reason = Host_interrupted; _ } -> ()
                       | Prepared | Running | Terminal _
                       | Interrupted { reason = Cancelled; _ } ->
                         failwith
                           "actual H2 failed allocation did not retain interrupted H");
                      [%test_eq: int] 1 !(fixture.direct_providers);
                      assert_attempts
                        subject
                        ~source_delta:3L
                        ~remote_delta:2L
                        ~pending:1L;
                      let before_late = state subject.parent in
                      let h1_before_late = S.row before_late first.handle in
                      assert (
                        not
                          (List.exists
                             (Observation.Attempt_record.observations
                                (Ledger.Row.record h1_before_late))
                             ~f:(fun incoming ->
                               Observation.Key.equal
                                 (Observation.key incoming)
                                 (Observation.key usage))));
                      let before_correction_book = book_json (state subject.child) in
                      let correction_wait_reads = ref 0 in
                      fault_initial
                        ~after:(fun () ->
                          Read.arm read (fun () ->
                            Read.clear read;
                            let current = slot (state subject.child) first.stream in
                            assert (
                              Option.exists
                                current.pending
                                ~f:(Book.Pending.equal pending));
                            assert (
                              Jsonaf.exactly_equal
                                before_correction_book
                                (book_json (state subject.child)));
                            Int.incr correction_wait_reads))
                        ();
                      Model.within
                        env
                        "selected H1 sweep retains H2 initial error and Awaiting"
                        (fun () ->
                           rejected (fun () ->
                             Graph.on_observation h1 usage;
                             Ok ()));
                      S.Failure.raise_if_present read.failure;
                      [%test_eq: int] 2 !h2_initial_faults;
                      [%test_eq: int] 1 !correction_wait_reads;
                      assert (
                        Jsonaf.exactly_equal
                          before_correction_book
                          (book_json (state subject.child)));
                      assert (Actor.inference_usage_cleanup_pending subject.parent.actor);
                      let late_source = state subject.parent in
                      [%test_eq: int64]
                        Int64.(Ledger.revision before_late.inference_ledger + 1L)
                        (Ledger.revision late_source.inference_ledger);
                      assert (
                        List.exists
                          (Observation.Attempt_record.observations
                             (Ledger.Row.record (S.row late_source first.handle)))
                          ~f:(Observation.equal usage));
                      let late_projection = projection late_source first.stream in
                      assert (
                        Int64.(
                          PC.Projection.session_revision late_projection
                          > PC.Projection.session_revision initial_projection));
                      let source_before_fresh = ledger_json late_source in
                      let fresh =
                        new_graph
                          "614-initial-first-h3"
                          ~on_admitted:(fun ~scope ~accounting_id:_ ->
                            third_scope := Some scope)
                      in
                      fault_initial ();
                      Model.within
                        env
                        "fresh H3 rejected before allocation while H2 initial fails"
                        (fun () ->
                           rejected (fun () ->
                             run fresh ~on_observation:(Graph.on_observation fresh)));
                      S.Failure.raise_if_present read.failure;
                      [%test_eq: int] 3 !h2_initial_faults;
                      assert (
                        Jsonaf.exactly_equal
                          before_correction_book
                          (book_json (state subject.child)));
                      assert (
                        Jsonaf.exactly_equal
                          source_before_fresh
                          (ledger_json (state subject.parent)));
                      [%test_eq: int]
                        2
                        (List.length
                           (Model.new_rows subject.before_parent (state subject.parent)));
                      [%test_eq: int] 1 !(fixture.direct_providers);
                      assert (Option.is_none !third_scope);
                      assert_attempts
                        subject
                        ~source_delta:3L
                        ~remote_delta:2L
                        ~pending:1L;
                      let initial_reads = ref 0 in
                      let correction_reads = ref 0 in
                      let prewriter_reads = ref 0 in
                      let after_initial () =
                        Read.arm read (fun () ->
                          let source = state subject.parent in
                          let current = slot (state subject.child) first.stream in
                          [%test_eq: int]
                            2
                            (List.length (Model.new_rows subject.before_parent source));
                          match current.pending with
                          | None ->
                            [%test_eq: int64]
                              2L
                              (PC.Projection.admitted_attempts current.projection);
                            if Int.equal !correction_reads 0
                            then same_projection initial_projection current.projection;
                            if Int.equal !correction_reads 0
                            then
                              assert_attempts
                                subject
                                ~source_delta:3L
                                ~remote_delta:3L
                                ~pending:0L;
                            Int.incr correction_reads
                          | Some next ->
                            [%test_eq: int64] 3L (Book.Pending.next_prefix next);
                            assert (not (Book.Pending.equal pending next));
                            [%test_eq: int64]
                              2L
                              (PC.Projection.admitted_attempts current.projection);
                            same_projection late_projection current.projection;
                            [%test_eq: int] 1 !(fixture.direct_providers);
                            assert (Option.is_none !third_scope);
                            assert_attempts
                              subject
                              ~source_delta:3L
                              ~remote_delta:3L
                              ~pending:1L;
                            Int.incr prewriter_reads;
                            Read.clear read)
                      in
                      Read.arm read (fun () ->
                        Read.clear read;
                        let current = slot (state subject.child) first.stream in
                        assert (
                          Option.exists current.pending ~f:(Book.Pending.equal pending));
                        [%test_eq: int64]
                          1L
                          (PC.Projection.admitted_attempts current.projection);
                        Int.incr initial_reads;
                        after_initial ());
                      Model.within
                        env
                        "fresh retry confirms H2 then corrects before genuine H3"
                        (fun () ->
                           run fresh ~on_observation:(Graph.on_observation fresh)
                           |> protocol_ok
                           |> ignore);
                      S.Failure.raise_if_present read.failure;
                      [%test_eq: int] 1 !initial_reads;
                      assert (!correction_reads > 0);
                      [%test_eq: int] 1 !prewriter_reads;
                      assert (read.excluded > 0 && read.excluded <= 128);
                      [%test_eq: int] 2 !(fixture.direct_providers);
                      assert !third_provider;
                      assert_attempts
                        subject
                        ~source_delta:4L
                        ~remote_delta:4L
                        ~pending:0L;
                      assert (
                        Option.is_none (slot (state subject.child) first.stream).pending);
                      S.assert_completed (S.row (state subject.parent) first.handle);
                      let h3 =
                        Model.new_rows before_h2 (state subject.parent)
                        |> List.filter ~f:(fun row ->
                          not (Ledger.Handle.equal h2 (Ledger.Row.handle row)))
                        |> Model.only "actual sole H3 after drain"
                      in
                      S.assert_completed h3;
                      [%test_eq: int]
                        3
                        (List.length
                           (Model.new_rows subject.before_parent (state subject.parent)));
                      ignore (!pre_canonical_reads : int);
                      Ok ()))
            |> protocol_ok;
            S.execute_async daemon fixture ~check_completion:(function
              | Succeeded (`String text) ->
                assert (
                  String.is_substring text ~substring:"real initial-first Async worker")
              | completion -> raise_s [%sexp (completion : P.Completion.t)]);
            fixture.release_child ();
            let final_child =
              Model.within
                env
                "initial-first actual child and callbacks settle"
                (fun () ->
                   let rec await () =
                     S.Failure.raise_if_present fixture.failure;
                     let current = Model.checkpoint subject.child in
                     if Option.is_none current.active_operation
                     then current
                     else (
                       Eio.Fiber.yield ();
                       await ())
                   in
                   await ())
            in
            [%test_eq: int] 2 !(fixture.direct_providers);
            [%test_eq: int] 1 !(fixture.child_providers);
            [%test_eq: int64] subject.ceiling (Model.committed final_child);
            assert_attempts subject ~source_delta:4L ~remote_delta:4L ~pending:0L;
            assert (List.is_empty (Book.Subtotal.pending (Model.subtotal final_child)));
            assert (not (Actor.inference_usage_cleanup_pending subject.parent.actor));
            [%test_eq: int]
              3
              (List.length (Model.new_rows subject.before_parent (state subject.parent)));
            S.Failure.raise_if_present read.failure));
  print_endline
    "actual H2 initial blocks H1 correction; exact drain precedes one fresh H3";
  [%expect
    {| actual H2 initial blocks H1 correction; exact drain precedes one fresh H3 |}]
;;
