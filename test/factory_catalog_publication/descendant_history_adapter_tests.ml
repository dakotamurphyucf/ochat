open! Core
open Agent_server_test_support
open Retry_support
module Birth = Descendant_goal_birth_tests
module Goal = Goal_service_tests
module G = P.Goal
module GD = A.Goal_descendants
module O = Agent_store.Delegation_goal_origin
module D = Agent_store.Delegation_store
module L = A.Inference_ledger
module H = P.History_edit

let within ?(seconds = 10.) env phase f =
  try Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) seconds f with
  | Eio.Time.Timeout -> failwith ("568 history timed out: " ^ phase)
;;

module Probe = Birth.Provider_probe

let archive_equal first second =
  Sexp.equal
    (A.Session_state.Compaction_archive.sexp_of_t first)
    (A.Session_state.Compaction_archive.sexp_of_t second)
;;

let with_child ?(wrap_env = Fn.id) ?(attempt_ceiling = 5L) f =
  let active = ref None in
  let check_input = ref (fun (_ : Openai.Responses.Item.t list) -> ()) in
  let failure = ref None in
  let calls = ref 0 in
  let budget =
    G.Budget.create
      ~admitted_turns:None
      ~admitted_attempts:(Some attempt_ceiling)
      ~observed_tokens:None
      ~deadline:None
    |> protocol_ok
  in
  (* Budget the complete native birth plus three genuine durable child Turns;
     individual read/writer barriers keep their short phase deadlines. *)
  Birth.with_native_goal_child
    ~timeout:90.
    ~wrap_env
    ~budget
    ~child_sources:Birth.attempt_sources
    ~post_creation_stream:(fun inputs ->
      Option.iter !active ~f:(fun probe ->
        try
          !check_input inputs;
          Probe.before_submit probe;
          Int.incr calls
        with
        | exn ->
          failure := Some (exn, Stdlib.Printexc.get_raw_backtrace ());
          raise exn);
      let text =
        match !calls with
        | 0 -> "568 native child creation completed"
        | 1 -> "568 seed assistant suffix"
        | 2 -> "568 edited assistant suffix"
        | _ -> "568 continued assistant suffix"
      in
      completed_response text)
    (fun env sw daemon parent child scope reference counters ->
       Factory.prepare_session_start (Daemon.factory daemon) child |> protocol_ok;
       Owner.ensure_loaded_authenticated child.runtime ~actor:original |> protocol_ok;
       let writer, _ =
         Actor.attach child.actor ~mode:Read_write ~subscribe:false |> protocol_ok
       in
       let attached = ref true in
       let detach () =
         if !attached
         then (
           Actor.detach child.actor writer.id |> protocol_ok;
           attached := false)
       in
       protect_cleanup ~finally:detach (fun () ->
         Actor.start child.actor ~actor:original ~attachment_id:writer.id
         |> protocol_ok
         |> ignore;
         Owner.drain_idle_moderator child.runtime |> protocol_ok |> ignore;
         let current =
           Actor.with_moderator_checkpoint child.actor (fun () -> Actor.state child.actor)
           |> protocol_ok
         in
         assert (Option.is_none current.active_operation && Option.is_none current.failure);
         assert (Option.is_none current.moderator);
         (match current.lifecycle.observed with
          | Idle -> ()
          | Stopped
          | Queued_for_slot
          | Starting
          | Recovering
          | Running_turn _
          | Compacting _
          | Waiting_for_permission _
          | Stopping
          | Failed _ -> failwith "actual history child did not reach Idle");
         (match current.runtime_initialization with
          | Ready -> ()
          | Pending _ -> failwith "actual history child is not initialized");
         let parent_attempts_before =
           A.Goal_accounting.admitted_attempts (Goal.accounting (state parent))
         in
         [%test_eq: int64] 2L parent_attempts_before;
         let expect ~turns ~attempts ~text ~forbidden =
           assert (Option.is_none !active);
           let probe =
             Probe.create_with_counts
               ~turns
               ~attempts
               ~attempt_ceiling
               ~parent
               ~child
               ~scope
               ~reference
               ~parent_attempts_before
           in
           (check_input
            := fun inputs ->
                 let json = List.map inputs ~f:Openai.Responses.Item.jsonaf_of_t in
                 let contains text =
                   List.exists json ~f:(fun item ->
                     String.is_substring (Jsonaf.to_string item) ~substring:text)
                 in
                 assert (contains "554 actual inherited foreground inference");
                 assert (
                   List.exists json ~f:(fun item ->
                     match Jsonaf.member "role" item with
                     | Some (`String "user") ->
                       String.is_substring (Jsonaf.to_string item) ~substring:text
                     | None | Some _ -> false));
                 List.iter forbidden ~f:(fun text -> assert (not (contains text))));
           active := Some probe;
           probe
         in
         let quiet phase =
           let after =
             within ~seconds:30. env phase (fun () ->
               Goal_native_test_support.await_quiet child)
           in
           Owner.drain_idle_moderator child.runtime |> protocol_ok |> ignore;
           Actor.with_moderator_checkpoint child.actor (fun () -> Ok ()) |> protocol_ok;
           Option.iter !failure ~f:(fun (exn, backtrace) ->
             Exn.raise_with_original_backtrace exn backtrace);
           assert (Option.is_none after.failure);
           Option.value_exn !active |> Probe.after_terminal;
           active := None;
           state child
         in
         f
           env
           sw
           daemon
           parent
           child
           scope
           reference
           counters
           writer
           detach
           expect
           quiet
           calls))
;;

let operation probe =
  let _, handle = Option.value_exn probe.Probe.submitted in
  L.Handle.operation_id handle |> Option.value_exn
;;

let target child id =
  List.find_exn (state child).conversation.canonical_history ~f:(fun entry ->
    P.History.Id.equal entry.id id)
;;

let seed child writer expect quiet =
  let id = A.History_id_source.allocate child.Registry.history_ids |> protocol_ok in
  let entry =
    Owner.parse_user_content
      child.runtime
      ~id
      { kind = Plain_text; text = "568 original editable input"; attachments = [] }
    |> protocol_ok
    |> A.History_codec.to_protocol
  in
  let probe =
    expect ~turns:1L ~attempts:1L ~text:"568 original editable input" ~forbidden:[]
  in
  let submitted =
    Actor.submit_message_authenticated
      child.actor
      ~actor:original
      ~attachment_id:writer.P.Session.Attachment.id
      entry
    |> protocol_ok
  in
  let after = quiet "initial actual history provider" in
  assert (
    Option.equal P.Id.Operation.equal submitted.operation_id (Some (operation probe)));
  entry.id, after, probe
;;

let edit_request child writer id text key =
  let before = state child in
  let target = target child id in
  let edit =
    H.create
      ~history_id:id
      ~expected_content_revision:target.content_revision
      ~text
      ~mode:Edit_and_continue
    |> protocol_ok
  in
  H.Edit_request.
    { session_id = before.identity.session_id
    ; attachment_id = writer.P.Session.Attachment.id
    ; expected_generation = before.identity.generation
    ; expected_revision = before.counters.revision
    ; edit
    ; idempotency_key = P.Idempotency_key.of_string key |> protocol_ok
    }
;;

let started expected = function
  | H.Continuation.Started operation -> assert (P.Id.Operation.equal operation expected)
  | Not_requested | Not_started _ ->
    failwith "actual scoped history did not admit its Turn"
;;

module Root_barrier = struct
  module Kind = struct
    type t =
      | Reserve of P.Session_ref.t
      | Import of D.Reference.t
  end

  type t =
    { mutable target : (P.Id.Session.t * O.Scope.t * Kind.t) option
    ; mutable occurrence : A.Goal_descendant_occurrence.t option
    ; entered : unit Eio.Promise.t
    ; signal : unit Eio.Promise.u
    ; resume : unit Eio.Promise.t
    ; release : unit Eio.Promise.u
    }

  let create () =
    let entered, signal = Eio.Promise.create () in
    let resume, release = Eio.Promise.create () in
    { target = None; occurrence = None; entered; signal; resume; release }
  ;;

  let release t =
    t.target <- None;
    if Option.is_none (Eio.Promise.peek t.resume) then Eio.Promise.resolve t.release ()
  ;;

  let wrap t env =
    Crash_fault_io.wrap
      env
      ~boundary:Before_sync
      ~matches:(fun path ->
        Option.is_some t.target
        && String.is_substring path ~substring:"/journal/"
        && String.is_suffix path ~suffix:".log")
      ~reached:(fun path ->
        match t.target with
        | None -> ()
        | Some (parent, scope, kind) ->
          let scan =
            Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / path)
            |> Agent_store.Journal_segment.scan_contents
                 ~max_payload_length:(64 * 1024 * 1024)
            |> Source_preparation_tests.store_ok
          in
          (match List.last scan.entries with
           | Some frame when Int.equal (Agent_store.Frame.flags frame.frame) 0 ->
             let transaction =
               Agent_store.Transaction.decode (Agent_store.Frame.payload frame.frame)
               |> Source_preparation_tests.store_ok
             in
             if P.Id.Session.equal transaction.session_id parent
             then (
               let delta =
                 A.Session_delta_document.decode
                   ~limits:Document_schema.Limits.default
                   transaction.delta
                 |> document_ok
                 |> A.Session_delta_document.value
               in
               let occurrence =
                 Child_usage_fault_tests.Fault.leaves delta
                 |> List.find_map ~f:(function
                   | A.Session_delta.Goal_descendants_changed update ->
                     (match kind, GD.Update.reason update with
                      | Kind.Reserve session, Reserve { scope = actual; occurrence; _ }
                        when O.Scope.equal actual scope
                             && P.Session_ref.equal
                                  session
                                  (A.Goal_descendant_occurrence.Frame.session
                                     (A.Goal_descendant_occurrence.frame occurrence)) ->
                        (match A.Goal_descendant_occurrence.kind occurrence with
                         | Host_turn _ -> Some occurrence
                         | Attempt _ -> None)
                      | Kind.Import reference, Import actual
                        when D.Reference.equal reference actual ->
                        GD.branches
                          (GD.Update.accounting update)
                          ~goal_id:(O.Scope.goal_id scope)
                        |> List.find_map ~f:(fun branch ->
                          if
                            O.Scope.equal (GD.Branch.scope branch) scope
                            && D.Reference.equal (GD.Branch.reference branch) reference
                          then (
                            match GD.Branch.last branch with
                            | Some (Included included) ->
                              let occurrence =
                                A.Goal_descendant_included.pending included
                                |> A.Goal_descendant_pending.occurrence
                              in
                              (match A.Goal_descendant_occurrence.kind occurrence with
                               | Host_turn _ -> Some occurrence
                               | Attempt _ -> None)
                            | None | Some (No_writer_closed _) -> None)
                          else None)
                      | ( (Kind.Reserve _ | Kind.Import _)
                        , ( Register_new_root _
                          | Register_first_unrelated_child _
                          | Register_child _
                          | Register_branch _
                          | Register_retained_branch _
                          | Reserve _
                          | Record_included _
                          | Import _
                          | Close_before_writer _ ) ) -> None)
                   | _ -> None)
               in
               Option.iter occurrence ~f:(fun occurrence ->
                 t.occurrence <- Some occurrence;
                 t.target <- None;
                 Eio.Promise.resolve t.signal ();
                 Eio.Promise.await t.resume))
           | Some _ | None -> ()))
  ;;
end

let%expect_test
    "scoped edit-and-continue and Continue preserve actual archive and provider ownership"
  =
  let barrier = Root_barrier.create () in
  with_child
    ~wrap_env:(Root_barrier.wrap barrier)
    ~attempt_ceiling:6L
    (fun
        env
         sw
         _daemon
         parent
         child
         scope
         reference
         counters
         writer
         _detach
         expect
         quiet
         calls
       ->
       protect_cleanup
         ~finally:(fun () -> Root_barrier.release barrier)
         (fun () ->
            let id, before, _seed_probe = seed child writer expect quiet in
            let original = target child id in
            let edited_probe =
              expect
                ~turns:2L
                ~attempts:2L
                ~text:"568 revised editable input"
                ~forbidden:[ "568 original editable input"; "568 seed assistant suffix" ]
            in
            let result =
              Actor.edit_history
                child.actor
                (edit_request
                   child
                   writer
                   id
                   "568 revised editable input"
                   "568-real-edit")
              |> protocol_ok
            in
            let after = quiet "edited actual history provider" in
            started (operation edited_probe) result.continuation;
            assert (P.History.Id.equal result.history_id id);
            [%test_eq: int64]
              (Int64.succ (P.History.Content_revision.to_int64 original.content_revision))
              (P.History.Content_revision.to_int64 result.content_revision);
            [%test_eq: int64] before.counters.revision result.archived_revision;
            let edited = target child id in
            assert (
              P.History.Content_revision.equal
                edited.content_revision
                result.content_revision);
            let archive = List.hd_exn after.conversation.compaction_archives in
            assert (A.Session_state.Compaction_archive.equal_kind archive.kind Edit);
            [%test_eq: int64] result.archived_revision archive.revision;
            let archived =
              A.Compaction_archive.read
                ~env
                ~handle:(Option.value_exn child.store_handle)
                ~max_payload_length:(64 * 1024 * 1024)
                archive
              |> protocol_ok
            in
            assert (
              List.equal
                P.History.equal_entry
                before.conversation.canonical_history
                archived.conversation.canonical_history);
            let before_continue = state child in
            let continued_probe =
              expect
                ~turns:3L
                ~attempts:3L
                ~text:"568 revised editable input"
                ~forbidden:[ "568 original editable input"; "568 seed assistant suffix" ]
            in
            let request : H.Continue_request.t =
              { session_id = before_continue.identity.session_id
              ; attachment_id = writer.id
              ; expected_generation = before_continue.identity.generation
              ; expected_revision = before_continue.counters.revision
              ; idempotency_key =
                  P.Idempotency_key.of_string "568-real-continue" |> protocol_ok
              }
            in
            let continued = Actor.continue_history child.actor request |> protocol_ok in
            let final = quiet "continued actual history provider" in
            started (operation continued_probe) continued.continuation;
            assert (
              P.History.Content_revision.equal
                (target child id).content_revision
                edited.content_revision);
            assert (
              List.equal
                archive_equal
                after.conversation.compaction_archives
                final.conversation.compaction_archives);
            [%test_eq: int] 3 !calls;
            [%test_eq: int] 5 !(counters.providers);
            (* A fourth host Turn is durably accepted. Replacing its selected
               worker during the real ancestor Import cannot redirect that old
               admission. The spare attempt is eligibility, not an inference. *)
            let request =
              { request with
                expected_generation = final.identity.generation
              ; expected_revision = final.counters.revision
              ; idempotency_key =
                  P.Idempotency_key.of_string "568-replaced-history-worker" |> protocol_ok
              }
            in
            barrier.target
            <- Some
                 ( (state parent).identity.session_id
                 , scope
                 , Root_barrier.Kind.Import reference );
            let pending =
              Eio.Fiber.fork_promise ~sw (fun () ->
                Actor.continue_history child.actor request)
            in
            within env "actual ancestor Included import" (fun () ->
              Eio.Promise.await barrier.entered);
            (* Parent persistence is suspended; inspect only the free child mailbox. *)
            let accepted = state child in
            let accepted_operation = Option.value_exn accepted.active_operation in
            [%test_eq: P.Operation.kind] (Turn Administrative) accepted_operation.kind;
            (match accepted_operation.state with
             | Starting -> ()
             | Running | Cancelling | Completed | Failed _ | Cancelled | Interrupted _ ->
               failwith "History worker started before ancestor confirmation");
            let included =
              GD.local_included (Option.value_exn accepted.goal_descendants)
              |> List.hd_exn
            in
            let occurrence =
              A.Goal_descendant_included.pending included
              |> A.Goal_descendant_pending.occurrence
            in
            assert (
              A.Goal_descendant_occurrence.equal
                occurrence
                (Option.value_exn barrier.occurrence));
            (match A.Goal_descendant_occurrence.kind occurrence with
             | Host_turn { operation_id } ->
               assert (P.Id.Operation.equal operation_id accepted_operation.id)
             | Attempt _ -> failwith "History continuation was not a host Turn");
            assert (
              Option.is_some
                (L.find_turn_handle
                   accepted.inference_ledger
                   ~operation_id:accepted_operation.id
                   ~generation:accepted_operation.generation));
            let replacement_calls = ref 0 in
            let replacement =
              A.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
                Int.incr replacement_calls;
                A.Operation_worker.Completed
                  { final_history = input.history
                  ; runtime_requests = []
                  ; moderator_snapshot = None
                  })
            in
            Actor.set_operation_worker child.actor (Some replacement) |> protocol_ok;
            Root_barrier.release barrier;
            (match
               within env "replaced History handoff" (fun () ->
                 Eio.Promise.await_exn pending)
             with
             | Error failure ->
               [%test_eq: P.Error.code] Conflict failure.code;
               [%test_eq: string]
                 "history continuation runtime changed after acknowledgement"
                 failure.message
             | Ok _ -> failwith "replacement worker inherited an old History admission");
            let rejected =
              within env "acknowledged History terminal cleanup" (fun () ->
                Goal_native_test_support.await_quiet child)
            in
            assert (
              List.equal
                P.History.equal_entry
                final.conversation.canonical_history
                rejected.conversation.canonical_history);
            assert (
              List.equal
                archive_equal
                final.conversation.compaction_archives
                rejected.conversation.compaction_archives);
            let root = Probe.branch continued_probe (state parent) in
            assert (Option.is_none (GD.Branch.pending root));
            [%test_eq: int64] 7L (GD.Branch.confirmed_prefix root);
            (match GD.Branch.last root with
             | Some (Included actual) ->
               assert (A.Goal_descendant_included.equal included actual)
             | None | Some (No_writer_closed _) ->
               failwith "acknowledged History admission lost its Included receipt");
            assert (
              List.equal
                A.Goal_descendant_included.equal
                [ included ]
                (GD.local_included (Option.value_exn rejected.goal_descendants)));
            let observed =
              A.Child_contributions.snapshot
                (Option.value_exn (state parent).child_contributions)
                ~reference
              |> protocol_ok
              |> Option.value_exn
            in
            let marks =
              A.Child_usage_snapshot.own observed
              |> A.Child_usage_snapshot.Partition.marks
            in
            [%test_eq: int64] 4L marks.turn;
            [%test_eq: int64] 3L marks.attempt;
            [%test_eq: int64] 0L marks.untracked_turns;
            [%test_eq: int64] 0L marks.untracked_attempts;
            [%test_eq: int] 0 !replacement_calls;
            [%test_eq: int] 3 !calls;
            [%test_eq: int] 5 !(counters.providers)));
  print_endline
    "scoped history admitted once per command; exact prior archive and revised provider \
     input retained";
  [%expect
    {| scoped history admitted once per command; exact prior archive and revised provider input retained |}]
;;

let ledger_document ledger =
  match L.to_document ledger with
  | Ok document -> Document_schema.Document.to_string document
  | Error error -> raise_s [%sexp (error : L.Error.t)]
;;

let%expect_test
    "scoped history loses its original writer before child commit and closes only \
     reservation"
  =
  let barrier = Root_barrier.create () in
  with_child
    ~wrap_env:(Root_barrier.wrap barrier)
    (fun
        env
         sw
         _daemon
         parent
         child
         scope
         reference
         counters
         writer
         detach
         expect
         quiet
         calls
       ->
       protect_cleanup
         ~finally:(fun () -> Root_barrier.release barrier)
         (fun () ->
            let id, before, seed_probe = seed child writer expect quiet in
            let before_root = Probe.branch seed_probe (state parent) in
            let before_ledger = ledger_document before.inference_ledger in
            let request =
              edit_request child writer id "568 rejected edit" "568-stale-writer"
            in
            barrier.target
            <- Some
                 ( (state parent).identity.session_id
                 , scope
                 , Root_barrier.Kind.Reserve
                     (GD.session (Option.value_exn before.goal_descendants)) );
            let pending =
              Eio.Fiber.fork_promise ~sw (fun () ->
                Actor.edit_history child.actor request)
            in
            within env "actual ancestor reserve writer" (fun () ->
              Eio.Promise.await barrier.entered);
            (* Only the ancestor writer is suspended. The child's mailbox must be
             free to retire the genuine captured attachment before its commit. *)
            detach ();
            Root_barrier.release barrier;
            (match
               within env "stale history response" (fun () ->
                 Eio.Promise.await_exn pending)
             with
             | Error failure -> [%test_eq: P.Error.code] Conflict failure.code
             | Ok _ -> failwith "history used a writer retired during ancestor reserve");
            let rec closed () =
              let current = state parent in
              let actual =
                GD.branches
                  (Option.value_exn current.goal_descendants)
                  ~goal_id:Goal.goal_id
                |> List.find_exn ~f:(fun branch ->
                  D.Reference.equal (GD.Branch.reference branch) reference)
              in
              match GD.Branch.pending actual, GD.Branch.last actual with
              | None, Some (No_writer_closed pending) -> actual, pending
              | (Some _ | None), (None | Some (Included _ | No_writer_closed _)) ->
                Eio.Fiber.yield ();
                closed ()
            in
            let root, closed_pending = within env "factual no-writer close" closed in
            [%test_eq: int64]
              (GD.Branch.confirmed_prefix before_root)
              (GD.Branch.confirmed_prefix root);
            assert (
              A.Goal_descendant_occurrence.equal
                (Option.value_exn barrier.occurrence)
                (A.Goal_descendant_pending.occurrence closed_pending));
            assert (
              D.Reference.equal
                (A.Goal_descendant_pending.branch_reference closed_pending)
                reference);
            assert (O.Scope.equal (A.Goal_descendant_pending.scope closed_pending) scope);
            let after = state child in
            assert (Option.is_none after.active_operation && Option.is_none after.failure);
            assert (
              List.equal
                P.History.equal_entry
                before.conversation.canonical_history
                after.conversation.canonical_history);
            assert (
              List.equal
                archive_equal
                before.conversation.compaction_archives
                after.conversation.compaction_archives);
            [%test_eq: string] before_ledger (ledger_document after.inference_ledger);
            assert (
              List.equal
                A.Goal_descendant_included.equal
                (GD.local_included (Option.value_exn before.goal_descendants))
                (GD.local_included (Option.value_exn after.goal_descendants)));
            [%test_eq: int] 1 !calls;
            [%test_eq: int] 3 !(counters.providers)));
  print_endline
    "stale writer rejected before child edit and provider; genuine root reservation \
     closed without confirmation";
  [%expect
    {| stale writer rejected before child edit and provider; genuine root reservation closed without confirmation |}]
;;
