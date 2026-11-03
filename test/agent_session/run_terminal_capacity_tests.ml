open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module D = Document_schema
module Store = Agent_store
module Persistence = A.Session_persistence

let set = Goal_catalog_tests.set
let member = Goal_catalog_tests.member

let limits =
  D.Limits.create
    ~max_bytes:(8 * 1024 * 1024)
    ~max_depth:64
    ~max_fields:100_000
    ~max_nodes:200_000
  |> document_ok
;;

(* Explicit admitted historical custody, not runtime authorization. The exact
   pre-fix reproduction had these 200 non-job keys and the old 4096/live reserve. *)
let historical state =
  let index = Option.value_exn state.A.Session_state.run_state in
  let admitted = List.hd_exn (A.Run_state.runs index) in
  let schedules =
    List.init 200 ~f:(fun number ->
      let schedule : P.Schedule.t =
        { id = P.Id.Schedule.of_string (sprintf "sch_capacity_%d" number) |> protocol_ok
        ; session_id
        ; generation = admitted.source.generation
        ; payload = `Null
        ; created_at = timestamp
        ; next_due_at = timestamp
        ; misfire = Deliver_once_immediately
        ; status = Scheduled
        ; delivery_count = 0
        ; last_delivery_at = None
        ; ownership = None
        ; delivery_cancellation = None
        }
      in
      P.Schedule.validate schedule |> protocol_ok;
      schedule)
  in
  let work =
    List.map schedules ~f:(fun schedule ->
      P.Run_work.create
        ~key:(Retained (Schedule schedule.P.Schedule.id))
        ~generation:schedule.generation
      |> protocol_ok)
  in
  let run =
    P.Run.create
      ~id:admitted.id
      ~session:admitted.session
      ~principal_id:admitted.principal_id
      ~source:admitted.source
      ~mode:Workflow
      ~lifecycle:Active
      ~revision:(Int64.succ admitted.revision)
      ~owned_work:work
      ~relinquished_work:[]
      ~terminal_work:[]
      ~created_at:admitted.created_at
      ~updated_at:timestamp
    |> protocol_ok
  in
  let index = A.Run_state.advance index ~run ~intents:[] |> protocol_ok in
  let state = { state with run_state = Some index; schedules } in
  A.Session_state.validate state |> protocol_ok;
  state
;;

let pad_run document target =
  let json = D.Document.json document in
  let payload = member json "payload" in
  let run = member payload "run_state" in
  let empty = set run "future_capacity_padding" (`String "") in
  let count = target - String.length (Jsonaf.to_string empty) in
  assert (count > 0);
  let run = set run "future_capacity_padding" (`String (String.make count 'x')) in
  [%test_eq: int] (String.length (Jsonaf.to_string run)) target;
  set json "payload" (set payload "run_state" run)
  |> D.Document.inspect ~limits
  |> document_ok
;;

let recovery state =
  let delta =
    A.Goal_catalog_source.recovery_delta state ~now:timestamp
    |> protocol_ok
    |> Option.value_exn
  in
  A.Session_transition.apply ~now:timestamp state ~delta ~payloads:[] |> protocol_ok
;;

let%expect_test
    "known non-job proof frontier rejects before effects and truthful recovery fits"
  =
  Goal_run_tests.with_model (fun ~basis:_ ~held:_ ~admission:_ ~intent:_ ~delta:_ state ->
    let no_request = state in
    let state = Goal_run_tests.request_achievement state |> historical in
    let index = Option.value_exn state.run_state in
    let old =
      A.Run_job_capacity.bookkeeping_reserve index ~jobs:state.jobs |> protocol_ok
    in
    [%test_eq: int] old 4096;
    let document =
      A.Session_state_document.encode (A.Session_state_document.authored state) ~limits
      |> document_ok
    in
    let rejected = pad_run document (P.Run_limits.max_document_bytes - old - 64) in
    assert (Result.is_error (A.Session_state_document.decode ~limits rejected));
    (* Deliberately independent slack: it is larger than all 200 actual terminal
       proof encodings, rather than taking the implementation's reserve as an oracle. *)
    let incoming = pad_run document (P.Run_limits.max_document_bytes - old - 200_000) in
    let original = A.Session_state_document.decode ~limits incoming |> document_ok in
    let encoded = A.Session_state_document.encode original ~limits |> document_ok in
    assert (D.Json.equal (D.Document.json incoming) (D.Document.json encoded));
    let state = A.Session_state_document.value original in
    let transition = recovery state in
    let goal = Goal_run_tests.current_goal transition.state in
    assert (Option.is_some (P.Goal.request_settlement goal));
    assert (
      match P.Goal.state goal with
      | Open (Held Source_review) -> true
      | _ -> false);
    let terminal =
      Option.value_exn transition.state.run_state |> A.Run_state.runs |> List.hd_exn
    in
    [%test_eq: int] (List.length terminal.terminal_work) 200;
    assert (
      List.for_all terminal.terminal_work ~f:(fun proof ->
        P.Run_work.Terminal.equal_outcome proof.P.Run_work.Terminal.outcome Unconfirmed));
    assert (P.Run.Lifecycle.equal terminal.lifecycle (Terminal Interrupted));
    let admitted =
      A.Session_document_transition.admit
        original
        ~delta:transition.delta
        ~next:transition.state
        ~limits
      |> document_ok
    in
    let after = A.Session_state_document.encode admitted ~limits |> document_ok in
    assert (
      D.Json.equal
        (member
           (member (D.Document.payload incoming) "run_state")
           "future_capacity_padding")
        (member (member (D.Document.payload after) "run_state") "future_capacity_padding"));
    let actual_growth =
      String.length (Jsonaf.to_string (member (D.Document.payload after) "run_state"))
      - String.length
          (Jsonaf.to_string (member (D.Document.payload incoming) "run_state"))
    in
    assert (actual_growth > old && actual_growth < 200_000);
    let capacity =
      A.Session_state_document.terminal_capacity original ~document:incoming ~limits
      |> document_ok
    in
    assert (
      A.Capacity_growth.encoded_bytes (A.Run_terminal_document.growth capacity)
      >= actual_growth);
    let waiting_initial = historical no_request in
    let waiting_basis = Option.value_exn waiting_initial.run_state in
    let run = List.hd_exn (A.Run_state.runs waiting_basis) in
    let timer = List.hd_exn waiting_initial.schedules in
    let wake =
      P.Run_wake.create
        ~run_id:run.id
        ~source:run.source
        ~occurrence:
          (Delivered_timer
             { schedule_id = timer.id
             ; delivery_count = 1
             ; creator =
                 Moderator_event
                   (P.Id.Moderator_execution.of_string "mex_capacity_wake" |> protocol_ok)
             ; subscription = None
             })
      |> protocol_ok
    in
    let waiting =
      P.Run.create
        ~id:run.id
        ~session:run.session
        ~principal_id:run.principal_id
        ~source:run.source
        ~mode:run.mode
        ~lifecycle:(Waiting wake)
        ~revision:(Int64.succ run.revision)
        ~owned_work:run.owned_work
        ~relinquished_work:run.relinquished_work
        ~terminal_work:run.terminal_work
        ~created_at:run.created_at
        ~updated_at:timestamp
      |> protocol_ok
    in
    let waiting_index =
      A.Run_state.advance waiting_basis ~run:waiting ~intents:[] |> protocol_ok
    in
    let delivered =
      { timer with
        status = P.Schedule.Delivered
      ; delivery_count = 1
      ; last_delivery_at = Some timestamp
      }
    in
    let waiting_state =
      { waiting_initial with
        run_state = Some waiting_index
      ; schedules = delivered :: List.tl_exn waiting_initial.schedules
      }
    in
    let waiting_document =
      A.Session_state_document.encode
        (A.Session_state_document.authored waiting_state)
        ~limits
      |> document_ok
    in
    let waiting_payload = D.Document.payload waiting_document in
    let waiting_raw = member waiting_payload "run_state" in
    let rows =
      match member waiting_raw "runs" with
      | `Array [ row ] ->
        let lifecycle = member row "lifecycle" in
        let wake = member lifecycle "wake" in
        `Array
          [ set
              row
              "lifecycle"
              (set lifecycle "wake" (set wake "future_protected" (`String "retain")))
          ]
      | _ -> assert false
    in
    let protected =
      set
        (D.Document.json waiting_document)
        "payload"
        (set waiting_payload "run_state" (set waiting_raw "runs" rows))
      |> D.Document.inspect ~limits
      |> document_ok
    in
    assert (Result.is_error (A.Session_state_document.decode ~limits protected));
    let host_json = D.Document.json incoming in
    let padded = set host_json "future_host_padding" (`String "") in
    let host_size = String.length (Jsonaf.to_string padded) in
    let tight =
      D.Limits.create
        ~max_bytes:(host_size + 64)
        ~max_depth:64
        ~max_fields:100_000
        ~max_nodes:200_000
      |> document_ok
    in
    assert (
      Result.is_error
        (A.Session_state_document.decode
           ~limits:tight
           (D.Document.inspect ~limits:tight padded |> document_ok)));
    let steps =
      A.Run_state.terminal_capacity index
      |> protocol_ok
      |> A.Run_terminal_capacity.remaining_steps
    in
    [%test_eq: int] steps 201;
    let counters = state.counters in
    let near =
      { state with
        counters =
          { counters with transaction_sequence = Int64.(max_value - of_int steps) }
      }
    in
    A.Session_state.validate near |> protocol_ok;
    assert (
      Result.is_error
        (A.Session_state.validate
           { near with
             counters =
               { near.counters with
                 transaction_sequence = Int64.succ near.counters.transaction_sequence
               }
           }));
    let final = recovery near in
    A.Session_state.validate final.state |> protocol_ok;
    assert (
      Option.is_none
        (A.Goal_catalog_source.recovery_delta final.state ~now:timestamp |> protocol_ok)));
  print_endline
    "old 4096/live frontier rejects; admitted unknowns survive 200 real recovery proofs; \
     host/counters reserve";
  [%expect
    {| old 4096/live frontier rejects; admitted unknowns survive 200 real recovery proofs; host/counters reserve |}]
;;

let%expect_test
    "actual writer profile rejects unfunded future recovery before journal effects"
  =
  Goal_run_tests.with_model
    (fun ~basis:_ ~held:_ ~admission:_ ~intent:_ ~delta:_ state ->
       let state = Goal_run_tests.request_achievement state |> historical in
       { state with
         counters = { state.counters with transaction_sequence = 0L; event_sequence = 0L }
       })
    ~after_close:(fun env workspace initial ->
      let original = A.Session_state_document.authored initial in
      let encoded = A.Session_state_document.encode original ~limits |> document_ok in
      let terminal =
        A.Session_state_document.terminal_capacity original ~document:encoded ~limits
        |> document_ok
      in
      assert (
        Result.is_error
          (A.Session_terminal_journal.validate
             initial
             ~terminal
             ~limits
             ~max_payload_length:4096));
      A.Session_terminal_journal.validate
        initial
        ~terminal
        ~limits
        ~max_payload_length:1_048_576
      |> protocol_ok;
      let commit_with max_payload_length name ~recover =
        Eio.Switch.run (fun sw ->
          let directory =
            Filename.concat workspace.A.Workspace_instance.canonical_root.native_path name
          in
          let journal =
            Store.Journal.create
              ~env
              ~directory
              ~max_payload_length
              ~max_segment_bytes:4_194_304L
              ~max_segment_frames:16
            |> store_ok
          in
          let writer =
            Store.Commit_writer.create
              ~sw
              ~journal
              ~session_id
              ~next_transaction_sequence:1L
              ~previous_transaction_hash:None
              ~queue_capacity:8
            |> store_ok
          in
          [%test_eq: int]
            (Store.Commit_writer.max_payload_length writer)
            max_payload_length;
          let effects = ref 0 in
          let persistence =
            Persistence.create
              ~archive:(fun _ _ -> failwith "unexpected archive")
              ~pending_archive:None
              ~before_commit:
                (Some
                   (fun _ ->
                     Int.incr effects;
                     Ok ()))
              ~command_accepted:(fun _ _ -> ())
              ~writer
              ~durability:Flush
              ~limits
              ~archive_limits:limits
              ~retention_preflight:None
              ~restored:(Persistence.Restored.authored initial)
              ~previous_transaction_hash:None
          in
          Exn.protect
            ~finally:(fun () -> Store.Commit_writer.close writer)
            ~f:(fun () ->
              let transition =
                if recover
                then recovery initial
                else
                  A.Session_transition.apply
                    ~now:timestamp
                    initial
                    ~delta:(Batch [])
                    ~payloads:[]
                  |> protocol_ok
              in
              let result =
                Persistence.commit
                  persistence
                  ~command_audit:None
                  ~previous:initial
                  transition
              in
              if recover
              then (
                result |> protocol_ok;
                let restored =
                  Persistence.Restored.state (Persistence.restored persistence)
                in
                let run =
                  Option.value_exn restored.run_state |> A.Run_state.runs |> List.hd_exn
                in
                assert (P.Run.Lifecycle.equal run.lifecycle (Terminal Interrupted));
                assert (List.length run.terminal_work = 200);
                assert (Option.is_some (Persistence.transaction_hash persistence)))
              else (
                assert (Result.is_error result);
                [%test_eq: int64]
                  (Persistence.Restored.state (Persistence.restored persistence)).counters
                    .transaction_sequence
                  0L;
                assert (Option.is_none (Persistence.transaction_hash persistence));
                [%test_eq: int] !effects 0)))
      in
      commit_with 4096 "small-terminal-profile" ~recover:false;
      commit_with 1_048_576 "funded-terminal-profile" ~recover:true);
  print_endline
    "small actual Journal rejects before effects; funded writer commits truthful \
     200-proof recovery";
  [%expect
    {| small actual Journal rejects before effects; funded writer commits truthful 200-proof recovery |}]
;;

let%expect_test
    "actual single-turn admission cannot occupy another live Run's terminal keys"
  =
  Run_admission_tests.with_startup_actor
    (fun actor _ _ request start _ scope session attachment ->
       start (request "canonical-key-owner") |> protocol_ok |> ignore;
       let initial = A.Session_actor.state actor |> protocol_ok in
       let run = Option.value_exn initial.run_state |> A.Run_state.runs |> List.hd_exn in
       List.iter [ "run-terminal:"; "run-interrupted:" ] ~f:(fun namespace ->
         let before = A.Session_actor.state actor |> protocol_ok in
         let key =
           namespace
           ^ P.Id.Run.to_string run.id
           ^ ":"
           ^ Int64.to_string (Int64.succ run.revision)
         in
         (* Historical JSON with an occupied future key is admitted by the old
         schema but cannot promise genuine retirement. The same guard applies
         on decode; this JSON is not installed or used as execution authority. *)
         let raw = A.Run_state.to_jsonaf (Option.value_exn before.run_state) in
         let poisoned =
           match member raw "receipts" with
           | `Array (first :: rest) ->
             set raw "receipts" (`Array (set first "key" (`String key) :: rest))
           | _ -> assert false
         in
         assert (Result.is_error (A.Run_state.of_jsonaf poisoned));
         let request =
           P.Run_start.create
             ~session_id
             ~attachment_id:attachment.id
             ~generation:before.identity.generation
             ~expected_revision:before.counters.revision
             ~mode:Single_turn
             ~input:
               (User_submission { kind = Plain_text; text = "hello"; attachments = [] })
             ~key:(P.Idempotency_key.of_string key |> protocol_ok)
           |> protocol_ok
         in
         let result =
           A.Session_actor.admit_run
             actor
             ~scope
             ~request
             ~session
             ~request_sha256:(Goal_run_tests.digest (P.Run_start.to_json request))
             ~entry:(Some actor_entry)
         in
         assert (
           match result with
           | Error error ->
             String.equal
               error.P.Error.message
               "canonical Run terminal key is already occupied"
           | Ok _ -> false);
         let after = A.Session_actor.state actor |> protocol_ok in
         [%test_eq: int64] after.counters.revision before.counters.revision;
         [%test_eq: int]
           (List.length (A.Run_state.runs (Option.value_exn after.run_state)))
           1));
  print_endline
    "actual authenticated Single_turn rejects both foreign live-Run terminal namespaces \
     before dispatch";
  [%expect
    {| actual authenticated Single_turn rejects both foreign live-Run terminal namespaces before dispatch |}]
;;
