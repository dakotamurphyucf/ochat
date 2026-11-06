open! Core
open Agent_server_test_support
open Retry_support
module Resume = Descendant_goal_resume_tests
module Held = Descendant_held_input_tests
module Faults = Descendant_held_input_fault_tests
module Fault = Faults.Fault
module Existing = Source_preparation_tests
module Goal = Goal_service_tests
module Staging = Goal_resume_tests
module Runtime = Goal_resume_runtime_tests
module Direct = Descendant_turn_adapter_tests
module GD = A.Goal_descendants
module L = A.Inference_ledger
module S = A.Child_usage_snapshot
module D = Agent_store.Delegation_store
module Origin = Agent_store.Delegation_goal_origin
module Included = A.Goal_descendant_included
module Pending = A.Goal_descendant_pending

(* Journal decoders yield DATA only. Service results, the real writer boundary,
   and the recovered Factory import retain their existing authority owners. *)
let transactions env child =
  Existing.journal_entries env child
  |> List.filter_map ~f:(fun entry ->
    if Int.equal (Agent_store.Frame.flags entry.Agent_store.Journal.frame) 0
    then
      Some
        (Agent_store.Transaction.decode (Agent_store.Frame.payload entry.frame)
         |> Existing.store_ok)
    else None)
;;

let earlier_resume env child transaction receipt =
  transactions env child
  |> List.exists ~f:(fun (earlier : Agent_store.Transaction.t) ->
    Int64.(
      earlier.transaction_sequence
      < transaction.Agent_store.Transaction.transaction_sequence)
    && List.exists (Fault.delta earlier) ~f:(function
      | A.Session_delta.Goal_catalog_changed catalog ->
        List.exists
          (A.Goal_catalog.receipts catalog)
          ~f:(A.Goal_command_receipt.equal receipt)
      | _ -> false))
;;

let user_entries current =
  List.filter current.A.Session_state.conversation.canonical_history ~f:(fun entry ->
    P.History.equal_role entry.P.History.role User)
;;

let assert_pending parent scope reference included before =
  let actual = Direct.branch parent scope reference in
  [%test_eq: int64]
    (GD.Branch.confirmed_prefix before)
    (GD.Branch.confirmed_prefix actual);
  assert (
    Option.equal GD.Branch.Last.equal (GD.Branch.last before) (GD.Branch.last actual));
  assert (
    Option.exists
      (GD.Branch.pending actual)
      ~f:(Pending.equal (Included.pending included)))
;;

let run_child test_root =
  let fault = Fault.create () in
  Resume.with_episode
    ~test_root
    ~wrap_env:(Fault.wrap fault)
    (fun
        env
         _daemon
         parent
         child
         scope
         reference
         counters
         admission
         stopped
         _old_runtime
         _observe
         failure
         _live
         request
         call
       ->
       let before = Direct.branch parent scope reference in
       let parent_counts = Faults.own_counts (state parent) in
       let users = user_entries stopped in
       assert (not (List.is_empty users));
       [%test_eq: int64] 2L (GD.Branch.confirmed_prefix before);
       [%test_eq: int64 * int64] (1L, 1L) (Faults.own_counts stopped);
       [%test_eq: int] 3 !(counters.providers);
       fault.selected <- Some { session = stopped.identity.session_id; scope; reference };
       let receipt, primary =
         match
           Resume.within env "completed cold Resume Turn writer failure" call
           |> protocol_ok
         with
         | Actor.Goal_resume_result.Resume_failed { receipt; error } -> receipt, error
         | Uncertain _ -> failwith "known Resume command became uncertain"
         | Resumed _ | Rejected _ | Retained _ ->
           failwith
             "unknown canonical Resume Turn was presented as a completed continuation"
       in
       Staging.assert_receipt request stopped receipt;
       [%test_eq: P.Error.code] Persistence_error primary.code;
       [%test_eq: int] 1 fault.hits;
       let candidate = Option.value_exn fault.candidate in
       assert candidate.earlier_admission;
       assert (P.Run_receipt.equal admission candidate.admission);
       assert (earlier_resume env child candidate.transaction receipt);
       let current = state child in
       Runtime.assert_episode stopped current admission;
       let _, selected = Held.run current admission in
       assert (
         not
           (List.exists selected.owned_work ~f:(fun work ->
              Held.has_operation work candidate.operation)));
       assert (Option.is_none current.active_operation);
       assert (
         Option.is_none
           (L.find_turn_handle
              current.inference_ledger
              ~operation_id:candidate.operation.id
              ~generation:candidate.operation.generation));
       assert (List.equal P.History.equal_entry users (user_entries current));
       assert (
         List.equal
           Included.equal
           (GD.local_included (Option.value_exn stopped.goal_descendants))
           (GD.local_included (Option.value_exn current.goal_descendants)));
       let marks = Direct.marks child in
       [%test_eq: int64] 1L marks.turn;
       [%test_eq: int64] 1L marks.attempt;
       [%test_eq: int64 * int64] (1L, 1L) (Faults.own_counts current);
       [%test_eq: int] 3 !(counters.providers);
       Resume.raise_failure failure;
       assert_pending parent scope reference candidate.included before;
       [%test_eq: int64 * int64] parent_counts (Faults.own_counts (state parent));
       let cleanup =
         try
           Resume.within env "truthful failed cold Resume source close" (fun () ->
             Owner.close_and_wait child.runtime);
           failwith "unknown Resume Turn source was reported as fully retired"
         with
         | Owner.Cleanup_failed error -> error
       in
       [%test_eq: P.Error.code] Conflict cleanup.code;
       assert_pending parent scope reference candidate.included before;
       [%test_eq: int] 3 !(counters.providers);
       let report =
         `Object
           [ "primary", P.Error.to_json primary
           ; "cleanup", P.Error.to_json cleanup
           ; "reference", D.reference_to_jsonaf reference
           ; "scope", Origin.Scope.to_json scope
           ; "admission", P.Run_receipt.to_json admission
           ; "resume", A.Goal_command_receipt.to_json receipt
           ; "operation", P.Operation.to_json candidate.operation
           ; "included", Included.to_json candidate.included
           ; "users", `Array (List.map users ~f:P.History.entry_to_json)
           ; ( "transaction_sequence"
             , `String (Int64.to_string candidate.transaction.transaction_sequence) )
           ; ( "transaction_sha256"
             , `String (Agent_store.Transaction.hash candidate.transaction) )
           ; "prefix", `String (Int64.to_string (GD.Branch.confirmed_prefix before))
           ; "parent_turns", `String (Int64.to_string (fst parent_counts))
           ; "parent_attempts", `String (Int64.to_string (snd parent_counts))
           ]
       in
       (* Both the public callback and explicit strong-close failure have returned.
         This crash retires the process, not a falsely successful runtime close. *)
       Eio.Flow.copy_string (Jsonaf.to_string report ^ "\n") (Eio.Stdenv.stdout env);
       Core_unix.exit_immediately 73)
;;

let recover env sw root report =
  let member field = Jsonaf.member_exn field report in
  let reference = member "reference" |> D.reference_of_jsonaf |> Existing.store_ok in
  let scope = member "scope" |> Origin.Scope.of_json |> protocol_ok in
  let admission = member "admission" |> P.Run_receipt.of_json |> protocol_ok in
  let receipt = member "resume" |> A.Goal_command_receipt.of_json |> protocol_ok in
  let operation = member "operation" |> P.Operation.of_json |> protocol_ok in
  let included = member "included" |> Included.of_json |> protocol_ok in
  let users =
    member "users"
    |> Jsonaf.list_exn
    |> List.map ~f:(fun entry -> P.History.entry_of_json entry |> protocol_ok)
  in
  let number field = member field |> Jsonaf.string_exn |> Int64.of_string in
  let prefix = number "prefix" in
  let parent_counts = number "parent_turns", number "parent_attempts" in
  [%test_eq: int64] 2L prefix;
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
      completed_response "unexpected recovered Resume provider")
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
       let transaction =
         transactions env child
         |> List.filter ~f:(fun (transaction : Agent_store.Transaction.t) ->
           Int64.equal transaction.transaction_sequence (number "transaction_sequence"))
         |> function
         | [ transaction ] -> transaction
         | [] | _ :: _ :: _ -> failwith "synced Resume Turn missing after physical replay"
       in
       [%test_eq: string]
         (member "transaction_sha256" |> Jsonaf.string_exn)
         (Agent_store.Transaction.hash transaction);
       assert (earlier_resume env child transaction receipt);
       let current = state child in
       assert (List.equal P.History.equal_entry users (user_entries current));
       let catalog = Option.value_exn current.goal_catalog in
       assert (
         List.exists
           (A.Goal_catalog.receipts catalog)
           ~f:(A.Goal_command_receipt.equal receipt));
       let index, selected = Held.run current admission in
       let link = A.Run_state.goal_link index selected.id |> Option.value_exn in
       assert (P.Id.Goal.equal (A.Run_goal_link.goal_id link) Goal.goal_id);
       assert (
         P.Run_receipt.equal
           admission
           (P.Goal_episode.admission (A.Run_goal_link.episode link)));
       assert (
         List.exists selected.owned_work ~f:(fun work ->
           Held.has_operation work operation));
       assert (
         Option.is_some
           (L.find_turn_handle
              current.inference_ledger
              ~operation_id:operation.id
              ~generation:operation.generation));
       assert (
         List.equal
           Included.equal
           [ included ]
           (GD.local_included (Option.value_exn current.goal_descendants)));
       let marks = Direct.marks child in
       [%test_eq: int64] 2L marks.turn;
       [%test_eq: int64] 1L marks.attempt;
       [%test_eq: int64 * int64] (2L, 1L) (Faults.own_counts current);
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
             failwith "recovery lost factual Resume Included"));
       ignore (Child_usage_fault_tests.import recovered parent child |> protocol_ok);
       let actual =
         Direct.confirmed parent child scope reference ~turns:2L ~attempts:1L
       in
       assert (Included.equal actual included);
       let before = Direct.branch parent scope reference in
       [%test_eq: int64] 3L (GD.Branch.confirmed_prefix before);
       [%test_eq: int64 * int64] parent_counts (Faults.own_counts (state parent));
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
       let marks = S.Partition.marks (S.Goal_scope.total scoped |> protocol_ok) in
       [%test_eq: int64] 2L marks.turn;
       [%test_eq: int64] 1L marks.attempt;
       ignore (Child_usage_fault_tests.import recovered parent child |> protocol_ok);
       assert (GD.Branch.equal before (Direct.branch parent scope reference));
       [%test_eq: int64 * int64] parent_counts (Faults.own_counts (state parent));
       [%test_eq: int] 0 !providers)
;;

let%expect_test
    "completed uncertain cold Goal Resume survives caller unwind and physical recovery"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    Eio.Switch.run (fun sw ->
      let root = temporary_root env in
      let path = Eio.Path.(Eio.Stdenv.fs env / root) in
      protect_cleanup
        ~finally:(fun () -> Eio.Path.rmtree path)
        (fun () ->
           Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 240. (fun () ->
             let report = Faults.child_report ~mode:Goal_resume env root in
             recover env sw root report))));
  print_endline
    "known Resume receipt; completed unknown Turn retained; exact physical replay \
     imports once";
  [%expect
    {| known Resume receipt; completed unknown Turn retained; exact physical replay imports once |}]
;;
