open! Core
open Agent_server_test_support
open Retry_support
module Birth = Descendant_goal_birth_tests
module Held = Descendant_held_input_tests
module Direct = Descendant_turn_adapter_tests
module Existing = Source_preparation_tests
module Goal = Goal_service_tests
module Service = Agent_server.Goal_run_service
module GD = A.Goal_descendants
module L = A.Inference_ledger
module S = A.Child_usage_snapshot
module D = Agent_store.Delegation_store
module Origin = Agent_store.Delegation_goal_origin
module Included = A.Goal_descendant_included
module Pending = A.Goal_descendant_pending
module Occurrence = A.Goal_descendant_occurrence

(* All decoded values below are evidence DATA. Actual service Admission and
   actual recovered Factory observation remain the only runtime owners. *)
module Fault = struct
  type selected =
    { session : P.Id.Session.t
    ; scope : Origin.Scope.t
    ; reference : D.Reference.t
    }

  type candidate =
    { transaction : Agent_store.Transaction.t
    ; operation : P.Operation.t
    ; included : Included.t
    ; admission : P.Run_receipt.t
    ; ledger : L.t
    ; earlier_admission : bool
    }

  type t =
    { mutable selected : selected option
    ; mutable candidate : candidate option
    ; mutable hits : int
    }

  let create () = { selected = None; candidate = None; hits = 0 }

  let delta transaction =
    A.Session_delta_document.decode
      ~limits:Document_schema.Limits.default
      transaction.Agent_store.Transaction.delta
    |> document_ok
    |> A.Session_delta_document.value
    |> Child_usage_fault_tests.Fault.leaves
  ;;

  let decode frame =
    Agent_store.Transaction.decode
      (Agent_store.Frame.payload frame.Agent_store.Journal_segment.frame)
    |> Existing.store_ok
  ;;

  let inspect selected transaction entries =
    let changes = delta transaction in
    let record =
      List.find_map changes ~f:(function
        | A.Session_delta.Goal_descendants_changed update ->
          (match GD.Update.reason update with
           | Record_included { fact; receipts = [ included ] } ->
             let pending = Included.pending included in
             if
               Origin.Scope.equal selected.scope (Pending.scope pending)
               && D.Reference.equal selected.reference (Pending.branch_reference pending)
               && Occurrence.equal
                    (GD.Canonical.Fact.occurrence fact)
                    (Pending.occurrence pending)
             then (
               match
                 ( Occurrence.kind (Pending.occurrence pending)
                 , GD.Canonical.Fact.operation fact )
               with
               | Host_turn { operation_id }, Some operation
                 when P.Id.Operation.equal operation_id operation.id ->
                 Some (operation, included)
               | (Host_turn _ | Attempt _), (Some _ | None) -> None)
             else None
           | Record_included _
           | Register_new_root _
           | Register_first_unrelated_child _
           | Register_child _
           | Register_branch _
           | Register_retained_branch _
           | Reserve _
           | Import _
           | Close_before_writer _ -> None)
        | _ -> None)
    in
    let index =
      List.find_map changes ~f:(function
        | A.Session_delta.Run_state_changed index -> Some index
        | _ -> None)
    in
    let ledger =
      List.find_map changes ~f:(function
        | A.Session_delta.Inference_ledger_changed ledger
        | Delegated_inference_ledger_changed (ledger, _) -> Some ledger
        | _ -> None)
    in
    match record, index, ledger with
    | Some (operation, included), Some index, Some ledger ->
      let owners =
        A.Run_state.runs index
        |> List.filter ~f:(fun run ->
          List.exists run.P.Run.owned_work ~f:(fun work ->
            Held.has_operation work operation))
      in
      (match owners with
       | [ owner ] ->
         let link = A.Run_state.goal_link index owner.id |> Option.value_exn in
         assert (P.Id.Goal.equal (A.Run_goal_link.goal_id link) Goal.goal_id);
         let admission = P.Goal_episode.admission (A.Run_goal_link.episode link) in
         assert (P.Id.Run.equal admission.run_id owner.id);
         assert (
           List.exists (A.Run_state.receipts index) ~f:(P.Run_receipt.equal admission));
         assert (
           Option.is_some
             (L.find_turn_handle
                ledger
                ~operation_id:operation.id
                ~generation:operation.generation));
         let earlier_admission =
           List.exists entries ~f:(fun frame ->
             if
               Int.equal
                 (Agent_store.Frame.flags frame.Agent_store.Journal_segment.frame)
                 0
             then (
               let previous = decode frame in
               P.Id.Session.equal previous.session_id selected.session
               && Int64.(previous.transaction_sequence < transaction.transaction_sequence)
               && List.exists (delta previous) ~f:(function
                 | A.Session_delta.Run_state_changed previous ->
                   List.exists
                     (A.Run_state.receipts previous)
                     ~f:(P.Run_receipt.equal admission)
                 | _ -> false))
             else false)
         in
         assert earlier_admission;
         Some { transaction; operation; included; admission; ledger; earlier_admission }
       | [] | _ :: _ :: _ ->
         failwith "held Turn candidate did not have one actual Run owner")
    | (None | Some _), (None | Some _), (None | Some _) -> None
  ;;

  let wrap t env =
    Crash_fault_io.wrap
      env
      ~boundary:After_sync
      ~matches:(fun path ->
        String.is_substring path ~substring:"/journal/"
        && String.is_suffix path ~suffix:".log")
      ~reached:(fun path ->
        Option.iter t.selected ~f:(fun selected ->
          let scan =
            Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
            |> Agent_store.Journal_segment.scan_contents
                 ~max_payload_length:(64 * 1024 * 1024)
            |> Existing.store_ok
          in
          match List.last scan.entries with
          | Some frame when Int.equal (Agent_store.Frame.flags frame.frame) 0 ->
            let transaction = decode frame in
            if P.Id.Session.equal transaction.session_id selected.session
            then (
              match inspect selected transaction scan.entries with
              | None -> ()
              | Some candidate ->
                t.selected <- None;
                t.candidate <- Some candidate;
                t.hits <- t.hits + 1;
                raise (Core_unix.Unix_error (EIO, "held inherited Turn after sync", path)))
          | Some _ | None -> ()))
  ;;
end

let submitted_input (transaction : Agent_store.Transaction.t) =
  (* Held input is adopted through Pending_inputs_changed. Its public appended
     event supplies an independent exact entry to check against recovered state. *)
  transaction.durable_events
  |> List.concat_map ~f:(fun document ->
    let event =
      A.Durable_event_document.decode ~limits:Document_schema.Limits.default document
      |> document_ok
      |> A.Durable_event_document.value
    in
    match
      P.Event.Durable.Payload.of_json ~kind:event.kind event.payload |> protocol_ok
    with
    | History_appended entries -> entries
    | _ -> [])
  |> List.filter ~f:(fun entry ->
    String.is_substring
      (Jsonaf.to_string (P.History.entry_to_json entry))
      ~substring:(Held.Consumer.input Goal))
  |> function
  | [ entry ] -> entry
  | [] | _ :: _ :: _ -> failwith "held Turn did not append exactly one submitted input"
;;

let own_counts current =
  let account = Goal.accounting current in
  A.Goal_accounting.admitted_turns account, A.Goal_accounting.admitted_attempts account
;;

let assert_pending parent scope reference included ~prefix =
  let branch = Direct.branch parent scope reference in
  [%test_eq: int64] prefix (GD.Branch.confirmed_prefix branch);
  assert (
    Option.exists
      (GD.Branch.pending branch)
      ~f:(Pending.equal (Included.pending included)));
  match GD.Branch.last branch with
  | None -> ()
  | Some (Included _ | No_writer_closed _) ->
    failwith "unacknowledged first Turn acquired a terminal root outcome"
;;

let run_child test_root =
  let fault = Fault.create () in
  Birth.with_native_goal_child
    ~test_root
    ~timeout:120.
    ~wrap_env:(Fault.wrap fault)
    (fun env _sw daemon parent child scope reference counters ->
       Held.within env "scripted child startup before uncertain writer" (fun () ->
         Direct.start_child daemon child);
       let call = Held.goal_call daemon child in
       let before_child = state child in
       let before_marks = Direct.marks child in
       let before_parent = state parent in
       let parent_counts = own_counts before_parent in
       let prefix = GD.Branch.confirmed_prefix (Direct.branch parent scope reference) in
       [%test_eq: int64] 0L prefix;
       [%test_eq: int64 * int64] (0L, 0L) (own_counts before_child);
       [%test_eq: int64] 0L before_marks.turn;
       [%test_eq: int64] 0L before_marks.attempt;
       [%test_eq: int] 2 !(counters.providers);
       fault.selected
       <- Some { session = before_child.identity.session_id; scope; reference };
       let admission, primary =
         match
           Held.within env "completed held Turn writer failure" call |> protocol_ok
         with
         | Service.Outcome.Retained _ ->
           failwith "fresh held Goal returned retained admission"
         | Attempted attempt ->
           assert (Option.is_none (Service.Attempt.no_effects attempt));
           (match Service.Attempt.result attempt |> protocol_ok with
            | Activation_failed { admission; error } -> admission, error
            | Admission_uncertain _ -> failwith "known Run Admission became uncertain"
            | Activated _ | Controlled _ | Settled _ | Retained _ ->
              failwith "unknown canonical Turn was presented as a completed activation")
       in
       [%test_eq: P.Error.code] Persistence_error primary.code;
       [%test_eq: int] 1 fault.hits;
       let candidate = Option.value_exn fault.candidate in
       assert candidate.earlier_admission;
       assert (P.Run_receipt.equal admission candidate.admission);
       let input = submitted_input candidate.transaction in
       let current = state child in
       assert (
         not
           (List.exists current.conversation.canonical_history ~f:(fun entry ->
              P.History.Id.equal entry.id input.id)));
       let index, selected = Held.run current admission in
       let link = A.Run_state.goal_link index selected.id |> Option.value_exn in
       assert (P.Id.Goal.equal (A.Run_goal_link.goal_id link) Goal.goal_id);
       assert (
         P.Run_receipt.equal
           admission
           (P.Goal_episode.admission (A.Run_goal_link.episode link)));
       assert (Option.is_none current.active_operation);
       assert (
         not
           (List.exists selected.owned_work ~f:(fun work ->
              Held.has_operation work candidate.operation)));
       assert (
         Option.is_none
           (L.find_turn_handle
              current.inference_ledger
              ~operation_id:candidate.operation.id
              ~generation:candidate.operation.generation));
       assert (
         List.is_empty (GD.local_included (Option.value_exn current.goal_descendants)));
       [%test_eq: int64 * int64] (0L, 0L) (own_counts current);
       let marks = Direct.marks child in
       [%test_eq: int64] before_marks.turn marks.turn;
       [%test_eq: int64] before_marks.attempt marks.attempt;
       assert (List.is_empty (L.rows candidate.ledger));
       [%test_eq: int] 2 !(counters.providers);
       assert_pending parent scope reference candidate.included ~prefix;
       [%test_eq: int64 * int64] parent_counts (own_counts (state parent));
       let cleanup =
         try
           Held.within env "truthful failed held source close" (fun () ->
             Owner.close_and_wait child.runtime);
           failwith "unknown held Turn source was reported as fully retired"
         with
         | Owner.Cleanup_failed error -> error
       in
       [%test_eq: P.Error.code] Conflict cleanup.code;
       assert_pending parent scope reference candidate.included ~prefix;
       [%test_eq: int] 2 !(counters.providers);
       let report =
         `Object
           [ "primary", P.Error.to_json primary
           ; "cleanup", P.Error.to_json cleanup
           ; "reference", D.reference_to_jsonaf reference
           ; "scope", Origin.Scope.to_json scope
           ; "admission", P.Run_receipt.to_json admission
           ; "operation", P.Operation.to_json candidate.operation
           ; "included", Included.to_json candidate.included
           ; ( "transaction_sequence"
             , `String (Int64.to_string candidate.transaction.transaction_sequence) )
           ; ( "transaction_sha256"
             , `String (Agent_store.Transaction.hash candidate.transaction) )
           ; "prefix", `String (Int64.to_string prefix)
           ; "parent_turns", `String (Int64.to_string (fst parent_counts))
           ; "parent_attempts", `String (Int64.to_string (snd parent_counts))
           ]
       in
       (* The public call and explicit close attempt have returned. A hard crash
          joins the remaining poisoned owner; it is not successful cleanup. *)
       Eio.Flow.copy_string (Jsonaf.to_string report ^ "\n") (Eio.Stdenv.stdout env);
       Core_unix.exit_immediately 73)
;;

let child_report env root =
  Eio.Switch.run (fun sw ->
    let manager = Eio.Stdenv.process_mgr env in
    let output, sink = Eio.Process.pipe ~sw manager in
    let diagnostics, errors = Eio.Process.pipe ~sw manager in
    let child =
      Eio.Process.spawn
        ~sw
        manager
        ~env:[| "PATH=/usr/bin:/bin" |]
        ~stdout:sink
        ~stderr:errors
        [ "./goal_native_fault_child.exe"; root; "held-goal-unknown" ]
    in
    Eio.Flow.close sink;
    Eio.Flow.close errors;
    let report, diagnostics =
      Eio.Fiber.pair
        (fun () -> Eio.Buf_read.take_all (Eio.Buf_read.of_flow ~max_size:32768 output))
        (fun () ->
           Eio.Buf_read.take_all (Eio.Buf_read.of_flow ~max_size:16384 diagnostics))
    in
    (match Eio.Process.await child with
     | `Exited 73 -> ()
     | `Exited code ->
       raise_s [%sexp "held fault process failed", (code : int), (diagnostics : string)]
     | `Signaled signal ->
       raise_s
         [%sexp "held fault process signaled", (signal : int), (diagnostics : string)]);
    if not (String.is_empty (String.strip diagnostics))
    then raise_s [%sexp "unexpected held fault process stderr", (diagnostics : string)];
    Jsonaf.of_string report)
;;

let recover env sw root report =
  let member field = Jsonaf.member_exn field report in
  let reference = member "reference" |> D.reference_of_jsonaf |> Existing.store_ok in
  let scope = member "scope" |> Origin.Scope.of_json |> protocol_ok in
  let admission = member "admission" |> P.Run_receipt.of_json |> protocol_ok in
  let operation = member "operation" |> P.Operation.of_json |> protocol_ok in
  let included = member "included" |> Included.of_json |> protocol_ok in
  let number field = member field |> Jsonaf.string_exn |> Int64.of_string in
  let prefix = number "prefix" in
  let parent_counts = number "parent_turns", number "parent_attempts" in
  [%test_eq: P.Error.code]
    Persistence_error
    (member "primary" |> P.Error.of_json |> protocol_ok).code;
  [%test_eq: P.Error.code]
    Conflict
    (member "cleanup" |> P.Error.of_json |> protocol_ok).code;
  let providers = ref 0 in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      Int.incr providers;
      completed_response "unexpected recovered held provider")
  in
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
       let parent =
         Registry.load (Daemon.registry recovered) reference.key.parent_session_id
         |> protocol_ok
       in
       let child =
         Registry.load (Daemon.registry recovered) reference.child_session_id
         |> protocol_ok
       in
       let selected_transaction =
         Existing.journal_entries env child
         |> List.filter_map ~f:(fun entry ->
           if Int.equal (Agent_store.Frame.flags entry.Agent_store.Journal.frame) 0
           then
             Some
               (Agent_store.Transaction.decode (Agent_store.Frame.payload entry.frame)
                |> Existing.store_ok)
           else None)
         |> List.filter ~f:(fun transaction ->
           Int64.equal transaction.transaction_sequence (number "transaction_sequence"))
         |> function
         | [ transaction ] -> transaction
         | [] | _ :: _ :: _ ->
           failwith "synced uncertain transaction missing after physical replay"
       in
       [%test_eq: string]
         (member "transaction_sha256" |> Jsonaf.string_exn)
         (Agent_store.Transaction.hash selected_transaction);
       let current = state child in
       let input = submitted_input selected_transaction in
       assert (
         List.equal
           P.History.equal_entry
           [ input ]
           (List.filter current.conversation.canonical_history ~f:(fun entry ->
              P.History.Id.equal entry.id input.id)));
       let index, selected = Held.run current admission in
       let link = A.Run_state.goal_link index selected.id |> Option.value_exn in
       assert (P.Id.Goal.equal (A.Run_goal_link.goal_id link) Goal.goal_id);
       assert (
         List.exists selected.owned_work ~f:(fun work ->
           Held.has_operation work operation));
       assert (
         Option.is_some
           (L.find_turn_handle
              current.inference_ledger
              ~operation_id:operation.id
              ~generation:operation.generation));
       assert (List.is_empty (L.rows current.inference_ledger));
       assert (
         List.equal
           Included.equal
           [ included ]
           (GD.local_included (Option.value_exn current.goal_descendants)));
       let marks = Direct.marks child in
       [%test_eq: int64] 1L marks.turn;
       [%test_eq: int64] 0L marks.attempt;
       [%test_eq: int64 * int64] (1L, 0L) (own_counts current);
       [%test_eq: int] 0 !providers;
       let branch = Direct.branch parent scope reference in
       (match GD.Branch.pending branch with
        | Some pending ->
          assert (Pending.equal pending (Included.pending included));
          [%test_eq: int64] prefix (GD.Branch.confirmed_prefix branch)
        | None ->
          [%test_eq: int64] (Int64.succ prefix) (GD.Branch.confirmed_prefix branch);
          (match GD.Branch.last branch with
           | Some (Included actual) -> assert (Included.equal actual included)
           | None | Some (No_writer_closed _) ->
             failwith "recovery lost factual Included outcome"));
       ignore (Child_usage_fault_tests.import recovered parent child |> protocol_ok);
       let actual =
         Direct.confirmed parent child scope reference ~turns:1L ~attempts:0L
       in
       assert (Included.equal actual included);
       let before = Direct.branch parent scope reference in
       [%test_eq: int64] (Int64.succ prefix) (GD.Branch.confirmed_prefix before);
       [%test_eq: int64 * int64] parent_counts (own_counts (state parent));
       let observed =
         A.Child_contributions.snapshot
           (Option.value_exn (state parent).child_contributions)
           ~reference
         |> protocol_ok
         |> Option.value_exn
       in
       assert (D.Reference.equal reference (S.reference observed));
       let scoped =
         S.goal_scopes observed
         |> Option.value_exn
         |> List.find_exn ~f:(fun scoped ->
           Origin.Scope.equal scope (S.Goal_scope.scope scoped))
       in
       [%test_eq: int64]
         1L
         (S.Partition.marks (S.Goal_scope.total scoped |> protocol_ok)).turn;
       [%test_eq: int64]
         0L
         (S.Partition.marks (S.Goal_scope.total scoped |> protocol_ok)).attempt;
       ignore (Child_usage_fault_tests.import recovered parent child |> protocol_ok);
       assert (GD.Branch.equal before (Direct.branch parent scope reference));
       [%test_eq: int64 * int64] parent_counts (own_counts (state parent));
       [%test_eq: int] 0 !providers)
;;

let%expect_test
    "completed uncertain held Goal Turn survives caller unwind and physical recovery"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    Eio.Switch.run (fun sw ->
      let root = temporary_root env in
      let path = Eio.Path.(Eio.Stdenv.fs env / root) in
      protect_cleanup
        ~finally:(fun () -> Eio.Path.rmtree path)
        (fun () ->
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 180. (fun () ->
             let report = child_report env root in
             recover env sw root report))));
  print_endline
    "known Goal Admission, completed unknown Turn, failed cleanup; physical replay \
     imports exactly once";
  [%expect
    {| known Goal Admission, completed unknown Turn, failed cleanup; physical replay imports exactly once |}]
;;
