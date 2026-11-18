open! Core
open Agent_server_test_support
open Retry_support
module Birth = Descendant_goal_birth_tests
module Direct = Descendant_turn_adapter_tests
module Ancestry = Descendant_ancestry_support
module Goal = Goal_service_tests
module Existing = Source_preparation_tests
module GD = A.Goal_descendants
module G = P.Goal
module L = A.Inference_ledger
module S = A.Child_usage_snapshot
module D = Agent_store.Delegation_store
module Origin = Agent_store.Delegation_goal_origin
module Included = A.Goal_descendant_included
module Pending = A.Goal_descendant_pending
module Occurrence = A.Goal_descendant_occurrence

let within env phase f =
  try Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. f with
  | Eio.Time.Timeout -> failwith ("554 root confirmation timed out: " ^ phase)
;;

module Boundary = struct
  type t =
    | Before_sync
    | After_sync

  let argument = function
    | Before_sync -> "root-confirmation-before-sync"
    | After_sync -> "root-confirmation-after-sync"
  ;;

  let io = function
    | Before_sync -> Crash_fault_io.Before_sync
    | After_sync -> Crash_fault_io.After_sync
  ;;
end

module Fault = struct
  type selected =
    { parent : P.Id.Session.t
    ; child : P.Session_ref.t
    ; scope : Origin.Scope.t
    ; reference : D.Reference.t
    }

  type candidate =
    { transaction : Agent_store.Transaction.t
    ; included : Included.t
    ; snapshot : S.t
    }

  type t =
    { boundary : Boundary.t
    ; mutable selected : selected option
    ; mutable candidate : candidate option
    ; mutable hits : int
    }

  let create boundary = { boundary; selected = None; candidate = None; hits = 0 }

  let delta transaction =
    A.Session_delta_document.decode
      ~limits:Document_schema.Limits.default
      transaction.Agent_store.Transaction.delta
    |> document_ok
    |> A.Session_delta_document.value
    |> Child_usage_fault_tests.Fault.leaves
  ;;

  let inspect selected transaction =
    if
      not
        (P.Id.Session.equal
           selected.parent
           transaction.Agent_store.Transaction.session_id)
    then None
    else
      delta transaction
      |> List.find_map ~f:(function
        | A.Session_delta.Goal_descendants_changed update ->
          (match GD.Update.reason update, GD.Update.children update with
           | Import reference, Replace children
             when D.Reference.equal reference selected.reference ->
             let snapshot =
               A.Child_contributions.snapshot children ~reference
               |> protocol_ok
               |> Option.value_exn
             in
             S.goal_scopes snapshot
             |> Option.bind ~f:(fun entries ->
               List.find_map entries ~f:(fun scoped ->
                 if not (Origin.Scope.equal selected.scope (S.Goal_scope.scope scoped))
                 then None
                 else
                   S.Goal_scope.included scoped
                   |> Option.bind ~f:(fun included ->
                     let pending = Included.pending included in
                     let occurrence = Pending.occurrence pending in
                     match Occurrence.kind occurrence with
                     | Host_turn _
                       when P.Session_ref.equal
                              selected.child
                              (Occurrence.Frame.session (Occurrence.frame occurrence)) ->
                       assert (Origin.Scope.equal selected.scope (Pending.scope pending));
                       assert (
                         D.Reference.equal
                           selected.reference
                           (Pending.branch_reference pending));
                       let branch =
                         GD.branches
                           (GD.Update.accounting update)
                           ~goal_id:(Origin.Scope.goal_id selected.scope)
                         |> List.find_exn ~f:(fun branch ->
                           Origin.Scope.equal selected.scope (GD.Branch.scope branch)
                           && D.Reference.equal
                                selected.reference
                                (GD.Branch.reference branch))
                       in
                       assert (Option.is_none (GD.Branch.pending branch));
                       [%test_eq: int64]
                         (Pending.next_prefix pending)
                         (GD.Branch.confirmed_prefix branch);
                       [%test_eq: int64]
                         (Pending.nonce pending)
                         (GD.Branch.last_nonce branch);
                       (match GD.Branch.last branch with
                        | Some (Included actual) ->
                          assert (Included.equal actual included)
                        | None | Some (No_writer_closed _) ->
                          failwith "selected Import did not confirm same native receipt");
                       Some { transaction; included; snapshot }
                     | Host_turn _ | Attempt _ -> None)))
           | _ -> None)
        | _ -> None)
  ;;

  let wrap t env =
    Crash_fault_io.wrap
      env
      ~boundary:(Boundary.io t.boundary)
      ~matches:(fun path ->
        Option.is_some t.selected
        && String.is_substring path ~substring:"/journal/"
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
            let transaction =
              Agent_store.Transaction.decode (Agent_store.Frame.payload frame.frame)
              |> Existing.store_ok
            in
            Option.iter (inspect selected transaction) ~f:(fun candidate ->
              t.selected <- None;
              t.candidate <- Some candidate;
              t.hits <- t.hits + 1;
              raise (Core_unix.Unix_error (EIO, "actual root confirmation", path)))
          | Some _ | None -> ()))
  ;;
end

let transactions env entry =
  Existing.journal_entries env entry
  |> List.filter_map ~f:(fun frame ->
    if Int.equal (Agent_store.Frame.flags frame.Agent_store.Journal.frame) 0
    then
      Some
        (Agent_store.Transaction.decode (Agent_store.Frame.payload frame.frame)
         |> Existing.store_ok)
    else None)
;;

let own_counts current =
  let account = Goal.accounting current in
  A.Goal_accounting.admitted_turns account, A.Goal_accounting.admitted_attempts account
;;

let marks partition ~turns ~attempts =
  let actual = S.Partition.marks partition in
  [%test_eq: int64] turns actual.turn;
  [%test_eq: int64] attempts actual.attempt;
  [%test_eq: int64] 0L actual.untracked_turns;
  [%test_eq: int64] 0L actual.untracked_attempts
;;

let assert_snapshot snapshot scope reference included =
  assert (D.Reference.equal reference (S.reference snapshot));
  Ancestry.assert_coverage (S.coverage snapshot);
  marks (S.own snapshot) ~turns:1L ~attempts:0L;
  marks (S.Descendants.partition (S.descendants snapshot)) ~turns:0L ~attempts:0L;
  let scoped =
    S.goal_scopes snapshot
    |> Option.value_exn
    |> function
    | [ scoped ] -> scoped
    | [] | _ :: _ :: _ -> failwith "actual confirmation lost its one root scope"
  in
  assert (Origin.Scope.equal scope (S.Goal_scope.scope scoped));
  assert (S.Partition.equal (S.own snapshot) (S.Goal_scope.own scoped));
  assert (Option.exists (S.Goal_scope.included scoped) ~f:(Included.equal included));
  Ancestry.assert_coverage (S.Goal_scope.coverage scoped)
;;

let assert_native child scope reference included message operation_id =
  let current = state child in
  assert (Option.exists current.spec.delegation ~f:(D.Reference.equal reference));
  assert (
    List.equal
      Included.equal
      [ included ]
      (GD.local_included (Option.value_exn current.goal_descendants)));
  assert (List.is_empty (L.rows current.inference_ledger));
  let occurrence = Included.pending included |> Pending.occurrence in
  (match Occurrence.kind occurrence with
   | Host_turn { operation_id = actual } ->
     assert (P.Id.Operation.equal operation_id actual)
   | Attempt _ -> failwith "unknown root confirmation selected an Attempt");
  let frame = Occurrence.frame occurrence in
  assert (
    P.Id.Session.equal
      current.identity.session_id
      (P.Session_ref.session_id (Occurrence.Frame.session frame)));
  let handle =
    L.find_turn_handle
      current.inference_ledger
      ~operation_id
      ~generation:(Occurrence.Frame.generation frame)
    |> Option.value_exn
  in
  let actual = A.Child_accounting.snapshot current |> protocol_ok |> Option.value_exn in
  assert_snapshot actual scope reference included;
  assert (Int64.(S.revision actual >= Occurrence.Frame.candidate_session_revision frame));
  let matching =
    List.filter current.conversation.canonical_history ~f:(fun entry ->
      P.History.Id.equal entry.id message.P.History.id)
  in
  assert (List.equal P.History.equal_entry [ message ] matching);
  handle
;;

let assert_pending parent scope reference included =
  let branch = Direct.branch parent scope reference in
  [%test_eq: int64] 0L (GD.Branch.confirmed_prefix branch);
  [%test_eq: int64] 1L (GD.Branch.last_nonce branch);
  assert (
    Option.exists
      (GD.Branch.pending branch)
      ~f:(Pending.equal (Included.pending included)));
  match GD.Branch.last branch with
  | None -> ()
  | Some (Included _ | No_writer_closed _) ->
    failwith "uncertain parent import acquired a native ACK"
;;

let canonical_transaction env child included =
  transactions env child
  |> List.filter ~f:(fun transaction ->
    List.exists (Fault.delta transaction) ~f:(function
      | A.Session_delta.Goal_descendants_changed update ->
        (match GD.Update.reason update with
         | Record_included { receipts; _ } ->
           List.exists receipts ~f:(Included.equal included)
         | Register_new_root _
         | Register_first_unrelated_child _
         | Register_child _
         | Register_branch _
         | Register_retained_branch _
         | Reserve _
         | Import _
         | Close_before_writer _ -> false)
      | _ -> false))
  |> function
  | [ transaction ] -> transaction
  | [] | _ :: _ :: _ ->
    failwith "child has no unique actual canonical inclusion transaction"
;;

let report_transaction transaction =
  `Object
    [ ( "sequence"
      , `String (Int64.to_string transaction.Agent_store.Transaction.transaction_sequence)
      )
    ; "hash", `String (Agent_store.Transaction.hash transaction)
    ]
;;

let run_child boundary test_root =
  let fault = Fault.create boundary in
  let writer_barrier = Direct.Writer_barrier.create () in
  Birth.with_native_goal_child
    ~test_root
    ~timeout:90.
    ~child_sources:Birth.attempt_sources
    ~wrap_env:(fun env ->
      Fault.wrap fault (Direct.Writer_barrier.wrap writer_barrier env))
    (fun env sw daemon parent child scope reference counters ->
       within env "join actual parent moderator checkpoint" (fun () ->
         Actor.with_moderator_checkpoint parent.actor (fun () -> Ok ()))
       |> protocol_ok;
       Ancestry.start_legacy daemon child;
       let parent_before = state parent in
       let account = Goal.accounting parent_before in
       let root_marks = Ancestry.root_marks parent_before in
       let ceiling =
         Int64.succ Int64.(A.Goal_accounting.admitted_turns account + root_marks.turn)
       in
       let budget =
         G.Budget.create
           ~admitted_turns:(Some ceiling)
           ~admitted_attempts:None
           ~observed_tokens:None
           ~deadline:None
         |> protocol_ok
       in
       Goal_admitted_guard_tests.command
         daemon
         parent
         ~key:"554-unknown-root-cap"
         (Change_budget (Goal.goal_id, budget))
       |> protocol_ok
       |> ignore;
       let root_counts = own_counts (state parent) in
       let providers = !(counters.providers) in
       [%test_eq: int] 2 providers;
       Ancestry.with_writer child (fun writer ->
         let message =
           Ancestry.message child "554 actual child ACK precedes unknown ROOT Import"
         in
         let before_child = state child in
         let child_session =
           P.Session_ref.create
             ~server_id:(Agent_store.Session_store.server_id (Daemon.store daemon))
             ~session_id:before_child.identity.session_id
         in
         fault.selected
         <- Some
              { parent = parent_before.identity.session_id
              ; child = child_session
              ; scope
              ; reference
              };
         writer_barrier.child <- Some before_child.identity.session_id;
         let pending =
           Eio.Fiber.fork_promise ~sw (fun () ->
             Actor.submit_message_authenticated
               child.actor
               ~actor:original
               ~attachment_id:writer.id
               message)
         in
         protect_cleanup
           ~finally:(fun () ->
             Direct.Writer_barrier.release writer_barrier;
             within env "join actual failed caller" (fun () ->
               Eio.Promise.await_exn pending)
             |> ignore)
           (fun () ->
              within env "actual child writer before native ACK" (fun () ->
                Eio.Promise.await writer_barrier.reached);
              (* Child writer suspended: read ONLY the free parent. *)
              let parent_pending = state parent in
              let branch = Direct.branch parent scope reference in
              let reservation = GD.Branch.pending branch |> Option.value_exn in
              [%test_eq: int64] 0L (GD.Branch.confirmed_prefix branch);
              [%test_eq: int64] 1L (GD.Branch.last_nonce branch);
              [%test_eq: int64] 1L (Pending.next_prefix reservation);
              assert (Origin.Scope.equal scope (Pending.scope reservation));
              assert (D.Reference.equal reference (Pending.branch_reference reservation));
              let operation_id = Option.value_exn writer_barrier.operation in
              (match Occurrence.kind (Pending.occurrence reservation) with
               | Host_turn { operation_id = actual } ->
                 assert (P.Id.Operation.equal operation_id actual)
               | Attempt _ -> failwith "pending root did not select actual child Turn");
              [%test_eq: int] providers !(counters.providers);
              Direct.Writer_barrier.release writer_barrier;
              let primary =
                match
                  within env "completed unknown root confirmation" (fun () ->
                    Eio.Promise.await_exn pending)
                with
                | Error failure -> failure
                | Ok _ -> failwith "unknown root Import was reported as confirmed"
              in
              [%test_eq: P.Error.code] Persistence_error primary.code;
              [%test_eq: int] 1 fault.hits;
              let candidate = Option.value_exn fault.candidate in
              assert (Pending.equal reservation (Included.pending candidate.included));
              assert_snapshot candidate.snapshot scope reference candidate.included;
              Existing.assert_same parent_pending parent;
              assert_pending parent scope reference candidate.included;
              let handle =
                assert_native
                  child
                  scope
                  reference
                  candidate.included
                  message
                  operation_id
              in
              [%test_eq: int64 * int64] root_counts (own_counts (state parent));
              [%test_eq: int] providers !(counters.providers);
              let cleanup =
                try
                  within
                    env
                    "truthful strong close with unavailable root confirmation"
                    (fun () -> Owner.close_and_wait child.runtime);
                  failwith "unknown root confirmation was reported as fully retired"
                with
                | Owner.Cleanup_failed failure -> failure
              in
              (* Unlike unknown CHILD-writer cases, this is a known child ACK and
               a poisoned ROOT canonical projection. Diagnose any earlier winner. *)
              [%test_eq: P.Error.code] Journal_corrupt cleanup.code;
              [%test_eq: string]
                "selection canonical projection is not current"
                cleanup.message;
              let still_retained =
                assert_native
                  child
                  scope
                  reference
                  candidate.included
                  message
                  operation_id
              in
              assert (
                P.Id.Operation.equal
                  (L.Turn_handle.operation_id handle)
                  (L.Turn_handle.operation_id still_retained));
              [%test_eq: int]
                (L.Turn_handle.generation handle)
                (L.Turn_handle.generation still_retained);
              assert (Option.is_none (state child).active_operation);
              Existing.assert_same parent_pending parent;
              assert_pending parent scope reference candidate.included;
              [%test_eq: int] providers !(counters.providers);
              let child_transaction =
                canonical_transaction env child candidate.included
              in
              let report =
                `Object
                  [ "mode", `String (Boundary.argument boundary)
                  ; "primary", P.Error.to_json primary
                  ; "cleanup", P.Error.to_json cleanup
                  ; "reference", D.reference_to_jsonaf reference
                  ; "scope", Origin.Scope.to_json scope
                  ; "included", Included.to_json candidate.included
                  ; ( "turn_operation"
                    , P.Id.Operation.to_json (L.Turn_handle.operation_id handle) )
                  ; ( "turn_generation"
                    , `String (Int.to_string (L.Turn_handle.generation handle)) )
                  ; "message", P.History.entry_to_json message
                  ; "parent_transaction", report_transaction candidate.transaction
                  ; "child_transaction", report_transaction child_transaction
                  ; "root_turns", `String (Int64.to_string (fst root_counts))
                  ; "root_attempts", `String (Int64.to_string (snd root_counts))
                  ; "turn_ceiling", `String (Int64.to_string ceiling)
                  ; "goal", G.to_json (Goal.goal parent_pending)
                  ]
              in
              (* Actual caller and explicit failed close returned. Only the process
               exit retires poisoned resources; this is not successful cleanup. *)
              Eio.Flow.copy_string
                (Jsonaf.to_string report ^ "\n")
                (Eio.Stdenv.stdout env);
              Core_unix.exit_immediately 73)))
;;

let child_report env root boundary =
  Eio.Switch.run (fun sw ->
    let manager = Eio.Stdenv.process_mgr env in
    let output, sink = Eio.Process.pipe ~sw manager in
    let diagnostics, errors = Eio.Process.pipe ~sw manager in
    let process =
      Eio.Process.spawn
        ~sw
        manager
        ~env:[| "PATH=/usr/bin:/bin" |]
        ~stdout:sink
        ~stderr:errors
        [ "./goal_native_fault_child.exe"; root; Boundary.argument boundary ]
    in
    Eio.Flow.close sink;
    Eio.Flow.close errors;
    let raw, diagnostics =
      Eio.Fiber.pair
        (fun () -> Eio.Buf_read.take_all (Eio.Buf_read.of_flow ~max_size:65536 output))
        (fun () ->
           Eio.Buf_read.take_all (Eio.Buf_read.of_flow ~max_size:16384 diagnostics))
    in
    (match Eio.Process.await process with
     | `Exited 73 -> ()
     | `Exited code ->
       raise_s
         [%sexp
           "root confirmation child failed"
         , (code : int)
         , (diagnostics : string)
         , (raw : string)]
     | `Signaled signal ->
       raise_s
         [%sexp
           "root confirmation child signaled"
         , (signal : int)
         , (diagnostics : string)
         , (raw : string)]);
    if not (String.is_empty (String.strip diagnostics))
    then
      raise_s [%sexp "unexpected root confirmation child stderr", (diagnostics : string)];
    Jsonaf.of_string raw)
;;

let check_transaction env entry report =
  let sequence =
    Jsonaf.member_exn "sequence" report |> Jsonaf.string_exn |> Int64.of_string
  in
  let expected_hash = Jsonaf.member_exn "hash" report |> Jsonaf.string_exn in
  let transaction =
    transactions env entry
    |> List.filter ~f:(fun transaction ->
      Int64.equal transaction.Agent_store.Transaction.transaction_sequence sequence)
    |> function
    | [ transaction ] -> transaction
    | [] | _ :: _ :: _ -> failwith "actual replay lacks the reported unique transaction"
  in
  [%test_eq: string] expected_hash (Agent_store.Transaction.hash transaction);
  assert (P.Id.Session.equal (state entry).identity.session_id transaction.session_id);
  transaction
;;

let assert_confirmed parent scope reference included =
  let branch = Direct.branch parent scope reference in
  assert (Option.is_none (GD.Branch.pending branch));
  [%test_eq: int64] 1L (GD.Branch.confirmed_prefix branch);
  [%test_eq: int64] 1L (GD.Branch.last_nonce branch);
  (match GD.Branch.last branch with
   | Some (Included actual) -> assert (Included.equal actual included)
   | None | Some (No_writer_closed _) ->
     failwith "physical replay lost exact root Included");
  let snapshot = Ancestry.snapshot (state parent) reference in
  assert_snapshot snapshot scope reference included;
  marks
    (S.Descendants.partition (Ancestry.root_descendants (state parent)))
    ~turns:1L
    ~attempts:0L
;;

let recover env sw root boundary report =
  let member field = Jsonaf.member_exn field report in
  [%test_eq: string] (Boundary.argument boundary) (member "mode" |> Jsonaf.string_exn);
  let reference = member "reference" |> D.reference_of_jsonaf |> Existing.store_ok in
  let scope = member "scope" |> Origin.Scope.of_json |> protocol_ok in
  let included = member "included" |> Included.of_json |> protocol_ok in
  let message = member "message" |> P.History.entry_of_json |> protocol_ok in
  let original_goal = member "goal" |> G.of_json |> protocol_ok in
  let number field = member field |> Jsonaf.string_exn |> Int64.of_string in
  let root_counts = number "root_turns", number "root_attempts" in
  let ceiling = number "turn_ceiling" in
  let operation_id =
    match Occurrence.kind (Pending.occurrence (Included.pending included)) with
    | Host_turn { operation_id } -> operation_id
    | Attempt _ -> failwith "reported root occurrence is not the actual native Turn"
  in
  [%test_eq: P.Error.code]
    Persistence_error
    (member "primary" |> P.Error.of_json |> protocol_ok).code;
  let cleanup = member "cleanup" |> P.Error.of_json |> protocol_ok in
  [%test_eq: P.Error.code] Journal_corrupt cleanup.code;
  [%test_eq: string] "selection canonical projection is not current" cleanup.message;
  let providers = ref 0 in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      Int.incr providers;
      completed_response "unexpected recovered root-confirmation provider")
  in
  let daemon =
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
    ~finally:(fun () -> Daemon.shutdown daemon |> protocol_ok)
    (fun () ->
       let parent =
         Registry.load (Daemon.registry daemon) reference.key.parent_session_id
         |> protocol_ok
       in
       (* Root load precedes child load: exact native confirmation is recovered
       from the ROOT journal, not created by an operator import afterward. *)
       let root_transaction =
         check_transaction env parent (member "parent_transaction")
       in
       let selected =
         Fault.
           { parent = reference.key.parent_session_id
           ; child =
               Occurrence.Frame.session
                 (Occurrence.frame (Pending.occurrence (Included.pending included)))
           ; scope
           ; reference
           }
       in
       let candidate = Fault.inspect selected root_transaction |> Option.value_exn in
       assert (Included.equal candidate.included included);
       assert_confirmed parent scope reference included;
       [%test_eq: int64 * int64] root_counts (own_counts (state parent));
       [%test_eq: int] 0 !providers;
       let child =
         Registry.load (Daemon.registry daemon) reference.child_session_id |> protocol_ok
       in
       let record =
         D.resolve (Agent_store.Session_store.delegations (Daemon.store daemon)) reference
         |> Existing.store_ok
       in
       assert (D.Reference.equal reference (D.reference record));
       assert (
         Option.exists record.admission.goal_origin ~f:(fun origin ->
           List.equal Origin.Scope.equal [ scope ] (Origin.scopes origin)));
       let actual_child_transaction =
         check_transaction env child (member "child_transaction")
       in
       let found = canonical_transaction env child included in
       [%test_eq: string]
         (Agent_store.Transaction.hash actual_child_transaction)
         (Agent_store.Transaction.hash found);
       let handle = assert_native child scope reference included message operation_id in
       assert (
         P.Id.Operation.equal
           (L.Turn_handle.operation_id handle)
           (member "turn_operation" |> P.Id.Operation.of_json |> protocol_ok));
       [%test_eq: int]
         (member "turn_generation" |> Jsonaf.string_exn |> Int.of_string)
         (L.Turn_handle.generation handle);
       [%test_eq: int] 0 !providers;
       (match Child_usage_fault_tests.import daemon parent child |> protocol_ok with
        | Imported (Replaced | Duplicate) -> ()
        | Imported (Added | Stale) | Historical_absence ->
          failwith
            "recovered known child slot did not refresh its actual current snapshot");
       assert_confirmed parent scope reference included;
       let actual =
         A.Child_accounting.snapshot (state child) |> protocol_ok |> Option.value_exn
       in
       [%test_eq: string]
         (S.digest actual)
         (S.digest (Ancestry.snapshot (state parent) reference));
       [%test_eq: int64 * int64] root_counts (own_counts (state parent));
       let goal = Goal.goal (state parent) in
       assert (P.Id.Goal.equal (G.id original_goal) (G.id goal));
       assert (P.Session_ref.equal (G.session original_goal) (G.session goal));
       assert (P.Run_source.equal (G.source original_goal) (G.source goal));
       assert (G.Budget.equal (G.budget original_goal) (G.budget goal));
       (* Recovery control can already be Held. This test attributes no fresh
       denial to quota merely because some admission would be denied. *)
       (match G.state goal with
        | Open (Enabled | Held _) -> ()
        | Closed _ -> failwith "unknown confirmation recovery closed the actual Goal");
       assert (
         Option.equal Int64.equal (Some ceiling) (G.Budget.admitted_turns (G.budget goal)));
       let current = state parent in
       let availability ceiling =
         GD.available
           (Option.value_exn current.goal_descendants)
           ~children:(Option.value_exn current.child_contributions)
           ~goal_id:Goal.goal_id
           ~local_count:(A.Goal_accounting.admitted_turns (Goal.accounting current))
           ~ceiling:(Some ceiling)
           ~dimension:Host_turn
         |> protocol_ok
       in
       assert (GD.Availability.equal Exhausted (availability ceiling));
       assert (GD.Availability.equal Available (availability (Int64.succ ceiling)));
       let before_duplicate = state parent in
       (match Child_usage_fault_tests.import daemon parent child |> protocol_ok with
        | Imported Duplicate -> ()
        | Imported (Added | Replaced | Stale) | Historical_absence ->
          failwith "same exact recovered snapshot was not Duplicate");
       Existing.assert_same before_duplicate parent;
       assert_confirmed parent scope reference included;
       [%test_eq: int64 * int64] root_counts (own_counts (state parent));
       [%test_eq: int] 0 !providers)
;;

let%expect_test
    "unknown root confirmation physically replays the same child ACK once at both sync \
     cuts"
  =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    Eio.Switch.run (fun sw ->
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 240. (fun () ->
        List.iter [ Boundary.Before_sync; After_sync ] ~f:(fun boundary ->
          let root = temporary_root env in
          let path = Eio.Path.(Eio.Stdenv.fs env / root) in
          protect_cleanup
            ~finally:(fun () -> Eio.Path.rmtree path)
            (fun () ->
               let report = child_report env root boundary in
               recover env sw root boundary report)))));
  print_endline
    "both root Import sync cuts: child ACK retained, physical confirmation once, no \
     provider or renewed count allowance";
  [%expect
    {| both root Import sync cuts: child ACK retained, physical confirmation once, no provider or renewed count allowance |}]
;;
