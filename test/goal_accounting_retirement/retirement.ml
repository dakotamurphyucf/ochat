open! Core
open Agent_server_test_support
open Factory_catalog_publication_tests
open Retry_support
module Goal = Goal_service_tests
module Admission = Goal_admission_fault_tests
module Accounting = A.Goal_accounting
module Ledger = A.Inference_ledger
module Usage = A.Goal_usage
module G = P.Goal
module Graph = Agent_server.Graph_tracking
module O = Inference.Observation

(* Local qualification only: report through the fixture's live Eio resources. *)
module Progress = struct
  type t =
    { now : unit -> Mtime.t
    ; write : string -> unit
    ; start : Mtime.t
    ; mutable phase : string
    ; mutable completed : int
    }

  let create ~now ~write =
    { now; write; start = now (); phase = "fixture ready"; completed = 0 }
  ;;

  let report t =
    let seconds = Mtime.Span.to_float_ns (Mtime.span t.start (t.now ())) /. 1e9 in
    t.write
      (sprintf
         "retirement: phase=%s completed=%d wall=%.1fs\n"
         t.phase
         t.completed
         seconds)
  ;;

  let phase t phase =
    t.phase <- phase;
    report t
  ;;

  let complete_admission t =
    t.completed <- t.completed + 1;
    if Int.equal (t.completed % 32) 0 then report t
  ;;
end

let accounting_ok result =
  Result.map_error result ~f:(fun error ->
    Sexp.to_string_hum (Accounting.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let observation_ok result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (O.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let input_usage current =
  Accounting.usage
    (Goal.accounting current)
    ~ledger:current.A.Session_state.inference_ledger
  |> accounting_ok
  |> fun usage -> Usage.component usage Input |> Usage.Metric.actual
;;

(* The caller retains the actual admission handle returned by this Goal's real
   provider. This is authoritative late host evidence, not a decoded scope or a
   second inference admission. Other components stay explicitly unknown. *)
let correction handle revision input =
  let actual = O.Count.create (Actual input) |> observation_ok in
  let unknown = O.Count.create (Unknown Not_reported) |> observation_ok in
  let counts =
    O.Usage.create
      ~counts:
        { input = actual
        ; output = unknown
        ; reported_total = unknown
        ; cached_input = unknown
        ; cache_write_input = unknown
        ; reasoning_output = unknown
        }
      ~inclusions:[]
    |> observation_ok
  in
  O.create
    ~scope:(Ledger.Handle.scope handle)
    ~id:(Ledger.Handle.accounting_id handle)
    ~revision
    ~payload:(Usage counts)
    ~limits:O.Admission.observation
  |> observation_ok
;;

let abandon entry session =
  let current = state entry in
  let goal = Goal.goal current in
  let request =
    A.Goal_command.create
      ~session_id:current.identity.session_id
      ~generation:current.identity.generation
      ~source:None
      ~expected_catalog_revision:
        (A.Goal_catalog.revision (Option.value_exn current.goal_catalog))
      ~expected_goal_revision:(Some (G.revision goal))
      ~key:(P.Idempotency_key.of_string "478-close-account" |> protocol_ok)
      ~mutation:
        (Abandon (G.id goal, G.Reason.of_string "Retain final accounting" |> protocol_ok))
    |> protocol_ok
  in
  Actor.goal_command entry.Registry.actor ~actor:original ~authorize ~session ~request
  |> protocol_ok
  |> ignore;
  let closed = state entry in
  (match G.state (Goal.goal closed) with
   | Closed _ -> ()
   | Open _ -> failwith "actual Abandon did not close the Goal");
  closed
;;

let preparation_ok result =
  Result.map_error result ~f:(fun error ->
    Sexp.to_string_hum (Inference_runtime.Preparation_error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

(* Ordinary, explicitly unbound host allocations supply real retirement pressure.
   Each actual Prepared admission is durably ACKed and immediately joined through
   Graph release, which truthfully records Definitely_not_submitted interruption.
   No pressure provider executes and no completion is fabricated. The closed Goal
   still has its actual provider/late usage. Durable profile stays 256 attempts. *)
let with_pressure_graph actor f =
  let backend_dispatches = ref 0 in
  let fixture =
    Inference_fixture.create
      ~namespace:"478-retirement-pressure"
      ~default_model:"fixture"
      ~post_stream:(fun ~sw:_ ~inputs:_ ->
        Int.incr backend_dispatches;
        Seq.empty)
  in
  let target =
    Inference_fixture.capture_config fixture Chat_response.Config.default
    |> preparation_ok
  in
  let context = Inference_fixture.resolve fixture target |> preparation_ok in
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
  let next = ref 0 in
  let upstream : Graph.Upstream.t =
    { new_preparation_id =
        (fun () ->
          Int.incr next;
          "478-pressure-" ^ Int.to_string !next)
    ; on_admitted = (fun ~scope:_ ~accounting_id:_ -> ())
    ; on_attempt = ignore
    ; on_observation = ignore
    ; on_completion = ignore
    }
  in
  let graph =
    Graph.create
      actor
      ~source:(Transcript.Source_id.of_string "478-unrelated" |> Result.ok_or_failwith)
      ~upstream
    |> protocol_ok
  in
  protect_cleanup
    ~finally:(fun () ->
      Graph.seal graph |> protocol_ok;
      Graph.finish graph |> protocol_ok)
    (fun () -> f graph context request backend_dispatches)
;;

let admit_unsubmitted ?(on_admitted = fun () -> ()) graph context request ~admissions =
  (* Cancellation/yield belongs between joined admissions, outside atomic custody. *)
  Eio.Fiber.check ();
  Eio.Fiber.yield ();
  let identity = Graph.identity graph in
  let prepared =
    Inference_runtime.Context.prepare
      context
      ~preparation_id:(identity.new_preparation_id ())
      request
    |> preparation_ok
  in
  identity.with_attempt
    prepared
    ~relation:Root
    ~f:(fun ~scope:_ ~accounting_id:_ ~submission_guard:_ ->
      Int.incr admissions;
      on_admitted ())
;;

let assert_unsubmitted rows =
  List.iter rows ~f:(fun row ->
    match O.Attempt_record.state (Ledger.Row.record row) with
    | Interrupted { reason = Host_interrupted; delivery = Definitely_not_submitted } -> ()
    | Prepared | Running | Terminal _ | Interrupted _ ->
      failwith "actual joined pressure admission was submitted or left active")
;;

let rec atoms = function
  | A.Session_delta.Batch changes -> List.concat_map changes ~f:atoms
  | delta -> [ delta ]
;;

let transactions env entry =
  Retry_tests.transaction_payloads env entry
  |> List.map ~f:(fun payload ->
    Agent_store.Transaction.decode payload |> Existing.store_ok)
;;

let delta transaction =
  A.Session_delta_document.decode
    ~limits:Document_schema.Limits.default
    transaction.Agent_store.Transaction.delta
  |> document_ok
  |> A.Session_delta_document.value
  |> atoms
;;

(* Snapshot pruning changes the retained journal prefix. Capture the fold's
   actual raw transaction immediately after its admission ACK, before release can
   make later checkpoints. No list-position cursor survives retention. *)
let retirement_transactions env entry ~write ~after_revision ~handle =
  let all = transactions env entry in
  let selected =
    List.filter all ~f:(fun transaction ->
      Int64.(transaction.Agent_store.Transaction.session_revision > after_revision))
  in
  let pairs =
    List.concat_map selected ~f:(fun transaction ->
      let changes = delta transaction in
      let ledgers =
        List.filter_map changes ~f:(function
          | A.Session_delta.Inference_ledger_changed ledger -> Some ledger
          | _ -> None)
      in
      List.filter_map changes ~f:(function
        | A.Session_delta.Goal_accounts_changed accounts ->
          Some (transaction, accounts, ledgers)
        | _ -> None))
  in
  let metadata (transaction : Agent_store.Transaction.t) =
    transaction.transaction_sequence, transaction.session_revision
  in
  let bounds values =
    Option.map (List.hd values) ~f:metadata, Option.map (List.last values) ~f:metadata
  in
  let summaries =
    List.take pairs 8
    |> List.map ~f:(fun (transaction, accounts, ledgers) ->
      let target_binding =
        Option.map (A.Goal_accounts.find accounts Goal.goal_id) ~f:(fun document ->
          A.Goal_accounting_document.stored document
          |> Accounting.Stored.ordinals
          |> List.exists ~f:(Int64.equal (Ledger.Handle.ordinal handle)))
      in
      let ledgers =
        List.map ledgers ~f:(fun ledger ->
          ( Ledger.revision ledger
          , List.length (Ledger.rows ledger)
          , Option.is_some (Ledger.find ledger ~ordinal:(Ledger.Handle.ordinal handle)) ))
      in
      metadata transaction, target_binding, ledgers)
  in
  write
    (Sexp.to_string_hum
       [%sexp
         "retirement raw journal proof"
       , (after_revision : int64)
       , (List.length all : int)
       , (bounds all : (int64 * int64) option * (int64 * int64) option)
       , (bounds selected : (int64 * int64) option * (int64 * int64) option)
       , (List.length pairs : int)
       , (summaries : ((int64 * int64) * bool option * (int64 * int * bool) list) list)
       , (Int.max 0 (List.length pairs - 8) : int)]
     ^ "\n");
  pairs
;;

let assert_folded before current handle closed_goal =
  assert (G.equal closed_goal (Goal.goal current));
  assert (
    Option.is_none
      (Ledger.find
         current.A.Session_state.inference_ledger
         ~ordinal:(Ledger.Handle.ordinal handle)));
  let account = Goal.accounting current in
  [%test_eq: int64] 1L (Accounting.admitted_attempts account);
  [%test_eq: int64] 1L (Accounting.admitted_turns account);
  [%test_eq: int64] 1L (Accounting.settled_tracked_attempts account);
  [%test_eq: int64] 0L (Accounting.untracked_attempts account);
  assert (List.is_empty (Accounting.bindings account));
  assert (P.Inference_query.Metric.equal_sum (Tokens 11L) (input_usage current));
  assert (
    Accounting.Scope.equal
      (Accounting.scope (Goal.accounting before))
      (Accounting.scope account))
;;

let run () =
  (* The default fixture remains 30s; this measured full-profile local case owns 900s. *)
  with_recoverable_fixture
    ~timeout:900.
    (fun env sw root daemon policy entry _runtime _revision counters ->
       let write text = Eio.Flow.copy_string text (Eio.Stdenv.stdout env) in
       let output text = write (text ^ "\n") in
       let progress =
         Progress.create
           ~now:(fun () -> Eio.Time.Mono.now (Eio.Stdenv.mono_clock env))
           ~write
       in
       let body () =
         Progress.phase progress "actual Goal create/admission";
         let session, created = Goal.create_goal daemon entry in
         let request = Admission.request created ~stale:false in
         let admission = Admission.call daemon entry session request |> Goal.activated in
         Progress.phase progress "await original provider/turn";
         let completed = Goal.await_input env entry counters in
         Progress.phase progress "close/correct exact retained Goal usage";
         let handle =
           match Accounting.bindings (Goal.accounting completed) with
           | [ binding ] -> Accounting.Binding.handle binding
           | _ -> failwith "real Goal provider did not return one canonical binding"
         in
         [%test_eq: int] 1 !(counters.providers);
         let closed = abandon entry session in
         let closed_goal = Goal.goal closed in
         let put revision tokens =
           Actor.observe_inference entry.actor ~handle (correction handle revision tokens)
           |> protocol_ok
         in
         let row =
           Ledger.find completed.inference_ledger ~ordinal:(Ledger.Handle.ordinal handle)
           |> Option.value_exn
         in
         let previous_usage_revision =
           O.Attempt_record.observations (Ledger.Row.record row)
           |> List.filter_map ~f:(fun observation ->
             match O.payload observation with
             | Usage _ -> Some (O.revision observation)
             | Configuration _ | Context_estimate _ | Transport_selection _ | Diagnostic _
               -> None)
           |> List.fold ~init:0L ~f:Int64.max
         in
         let first_revision = Int64.succ previous_usage_revision in
         let final_revision = Int64.succ first_revision in
         put first_revision 7L;
         put final_revision 11L;
         let corrected = state entry in
         assert (G.equal closed_goal (Goal.goal corrected));
         assert (P.Inference_query.Metric.equal_sum (Tokens 11L) (input_usage corrected));
         [%test_eq: int64] 1L (Accounting.admitted_attempts (Goal.accounting corrected));
         let frames = Existing.frames env entry in
         put final_revision 11L;
         put first_revision 7L;
         Existing.assert_same corrected entry;
         [%test_eq: string list] frames (Existing.frames env entry);
         (* Session revision remains stable across journal prefix pruning. *)
         let before_pressure_revision = corrected.counters.revision in
         let retirement = ref None in
         Progress.phase progress "open actual pressure graph";
         with_pressure_graph entry.actor (fun graph context request backend_dispatches ->
           let admissions = ref 0 in
           let measured_admission ?on_admitted () =
             admit_unsubmitted ?on_admitted graph context request ~admissions;
             Progress.complete_admission progress
           in
           Progress.phase progress "actual pressure admission/release";
           for _ = 1 to 255 do
             measured_admission ()
           done;
           Progress.phase
             progress
             "verify retained boundary after 255 pressure admissions";
           let retained = state entry in
           [%test_eq: int] 256 (List.length (Ledger.rows retained.inference_ledger));
           assert (
             Option.is_some
               (Ledger.find
                  retained.inference_ledger
                  ~ordinal:(Ledger.Handle.ordinal handle)));
           let pressure_rows =
             List.filter (Ledger.rows retained.inference_ledger) ~f:(fun row ->
               not (Ledger.Handle.equal handle (Ledger.Row.handle row)))
           in
           [%test_eq: int] 255 (List.length pressure_rows);
           assert_unsubmitted pressure_rows;
           [%test_eq: int64]
             0L
             (Accounting.settled_tracked_attempts (Goal.accounting retained));
           measured_admission
             ~on_admitted:(fun () ->
               retirement
               := Some
                    (retirement_transactions
                       env
                       entry
                       ~write
                       ~after_revision:before_pressure_revision
                       ~handle))
             ();
           [%test_eq: int] 256 !admissions;
           [%test_eq: int] 0 !backend_dispatches;
           Progress.phase progress "join pressure graph");
         Progress.phase progress "verify atomic retirement and replay denial";
         let retired = state entry in
         assert_unsubmitted (Ledger.rows retired.inference_ledger);
         assert_folded corrected retired handle closed_goal;
         let coverage =
           P.Inference_query.Summary.coverage (Ledger.summary retired.inference_ledger)
         in
         [%test_eq: int64] 1L coverage.retired_attempts;
         [%test_eq: int64] 0L coverage.untracked_attempts;
         [%test_eq: int] 256 (List.length (Ledger.rows retired.inference_ledger));
         [%test_eq: int] 1 !(counters.providers);
         (match !retirement with
          | Some [ (_transaction, accounts, [ ledger ]) ] ->
            assert (
              Option.is_none (Ledger.find ledger ~ordinal:(Ledger.Handle.ordinal handle)));
            assert (
              Jsonaf.exactly_equal
                (A.Goal_accounts.to_jsonaf accounts |> protocol_ok)
                (A.Goal_accounts.to_jsonaf (Option.value_exn retired.goal_accounts)
                 |> protocol_ok))
          | _ ->
            failwith "retirement did not atomically persist ledger and Goal fold once");
         let frames = Existing.frames env entry in
         put (Int64.succ final_revision) 99L;
         Existing.assert_same retired entry;
         [%test_eq: string list] frames (Existing.frames env entry);
         let old_store = Daemon.store daemon in
         Progress.phase progress "shutdown original daemon";
         Daemon.shutdown daemon |> protocol_ok;
         assert (Agent_store.Session_store.is_closed old_store);
         Progress.phase progress "physical daemon reopen";
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
           (fun () ->
              assert (not (phys_equal old_store (Daemon.store recovered)));
              let loaded =
                Registry.load (Daemon.registry recovered) retired.identity.session_id
                |> protocol_ok
              in
              assert (not (phys_equal entry.actor loaded.actor));
              Progress.phase progress "join actual recovered runtime";
              let current = Retry_rotation_tests.await_recovered loaded retired in
              Progress.phase progress "verify recovered totals/no resurrection";
              assert_folded corrected current handle closed_goal;
              assert (Accounting.equal (Goal.accounting retired) (Goal.accounting current));
              let old_run =
                A.Run_state.find (Option.value_exn current.run_state) admission.run_id
                |> Option.value_exn
              in
              (match old_run.lifecycle with
               | Terminal Interrupted -> ()
               | _ -> failwith "process recovery did not interrupt the old Workflow");
              [%test_eq: int] 1 !(counters.providers);
              let receipts = A.Run_state.receipts (Option.value_exn current.run_state) in
              let admissions =
                List.filter receipts ~f:(fun receipt ->
                  P.Id.Run.equal receipt.P.Run_receipt.run_id admission.run_id
                  && P.Run_receipt.Kind.equal Admission receipt.kind)
              in
              assert (List.equal P.Run_receipt.equal [ admission ] admissions);
              let frames = Existing.frames env loaded in
              Actor.observe_inference
                loaded.actor
                ~handle
                (correction handle Int64.(final_revision + 2L) 111L)
              |> protocol_ok;
              Existing.assert_same current loaded;
              [%test_eq: string list] frames (Existing.frames env loaded);
              output
                "real Goal provider; closed retained correction replaces once; unrelated \
                 work is uncharged";
              output
                "canonical retirement folds atomically; physical reopen retains totals \
                 and rejects resurrection");
         Progress.phase progress "outer source/daemon cleanup"
       in
       match body () with
       | () -> ()
       | exception exn ->
         let backtrace = Stdlib.Printexc.get_raw_backtrace () in
         (* Diagnostic failure cannot replace the actual assertion/cancellation. *)
         (try Progress.report progress with
          | _ -> ());
         Exn.raise_with_original_backtrace exn backtrace)
;;
