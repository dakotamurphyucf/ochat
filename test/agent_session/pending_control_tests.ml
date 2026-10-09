open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol

let entry =
  A.History_codec.user_text ~id:history_id "pending original"
  |> A.History_codec.to_protocol
;;

let project (entry : P.History.entry) =
  let open Result.Let_syntax in
  let%bind native = A.History_codec.of_protocol entry in
  P.Public_history.full
    native
    ~content_revision:entry.content_revision
    ~provenance:entry.provenance
;;

let seed ?(owner = A.Pending_input_document.Owner.Submitting_principal principal_id) state
  =
  let pending =
    P.Pending_input.create
      ~entry
      ~generation:state.A.Session_state.identity.generation
      ~binding:Agent_protocol.Pending_input.Binding.safe_boundary
    |> protocol_ok
    |> A.Pending_input_document.authored ~owner ~limits:document_limits
    |> document_ok
  in
  { state with
    conversation =
      { state.conversation with
        deferred_user_entries = [ pending ]
      ; next_history_sequence = 8L
      ; reserved_history_through = 8L
      }
  }
;;

let request state writer entry suffix =
  P.Pending_control.Cancel_request.create
    ~session_id:state.A.Session_state.identity.session_id
    ~attachment_id:writer.P.Session.Attachment.id
    ~expected_generation:state.identity.generation
    ~expected_pending_revision:state.conversation.pending_revision
    ~history_id:entry.P.History.id
    ~expected_content_revision:entry.content_revision
    ~idempotency_key:
      (P.Idempotency_key.of_string ("pending-control-" ^ suffix) |> protocol_ok)
  |> protocol_ok
;;

let cancel actor target =
  A.Session_actor.cancel_pending actor ~principal:principal_id ~project target
;;

let%expect_test
    "serialized replacement and cancellation preserve ownership and content CAS"
  =
  Job_fixtures.with_actor ~prepare_state:seed (fun _ _ actor writer _ ->
    let before = A.Session_actor.state actor |> protocol_ok in
    let original = request before writer entry "replace" in
    let replacement =
      P.Pending_control.Replace_request.create ~target:original ~text:"saved replacement"
      |> protocol_ok
    in
    let changed =
      A.Session_actor.replace_pending actor ~principal:principal_id ~project replacement
      |> protocol_ok
    in
    let saved = A.Session_actor.state actor |> protocol_ok in
    let wrapper = List.hd_exn saved.conversation.deferred_user_entries in
    let current = P.Pending_input.entry (A.Pending_input_document.value wrapper) in
    let stale = cancel actor original in
    let foreign = P.Id.Principal.of_string "pri_foreign_pending" |> protocol_ok in
    let fresh = request saved writer current "cancel" in
    let foreign_result =
      A.Session_actor.cancel_pending actor ~principal:foreign ~project fresh
    in
    let cancelled = cancel actor fresh |> protocol_ok in
    let repeated = cancel actor fresh |> protocol_ok in
    let after = A.Session_actor.state actor |> protocol_ok in
    printf
      "stable-id=%b revised-content=%b owner-preserved=%b stale-rejected=%b \
       foreign-rejected=%b\n"
      (P.History.Id.equal entry.id current.id)
      (Int64.equal 1L (P.History.Content_revision.to_int64 current.content_revision))
      (A.Pending_input_document.Owner.equal
         (Submitting_principal principal_id)
         (A.Pending_input_document.owner wrapper))
      (Result.is_error stale)
      (Result.is_error foreign_result);
    printf
      "replace-pending=%b cancel-winner=%b repeat-winner=%b queue=%d archives=%d \
       revisions-distinct=%b\n"
      (match changed.outcome with
       | Pending _ -> true
       | Adopted _ | Cancelled _ | Retired _ | Unavailable _ -> false)
      (match cancelled.outcome with
       | Cancelled id -> P.History.Id.equal id entry.id
       | Pending _ | Adopted _ | Retired _ | Unavailable _ -> false)
      (match repeated.outcome with
       | Cancelled id -> P.History.Id.equal id entry.id
       | Pending _ | Adopted _ | Retired _ | Unavailable _ -> false)
      (List.length after.conversation.deferred_user_entries)
      (List.length after.conversation.compaction_archives)
      (not
         (P.Pending_input.Revision.equal
            before.conversation.pending_revision
            after.conversation.pending_revision));
    [%expect
      {|stable-id=true revised-content=true owner-preserved=true stale-rejected=true foreign-rejected=true
replace-pending=true cancel-winner=true repeat-winner=true queue=0 archives=2 revisions-distinct=true|}])
;;

let%expect_test
    "adoption wins before stale cancellation and returns actual current occurrence"
  =
  Job_fixtures.with_actor ~prepare_state:seed (fun _ _ actor writer _ ->
    let before = A.Session_actor.state actor |> protocol_ok in
    let target = request before writer entry "adopt-winner" in
    A.Session_actor.adopt_deferred actor |> protocol_ok |> ignore;
    let adopted = A.Session_actor.state actor |> protocol_ok in
    let result = cancel actor target |> protocol_ok in
    let after = A.Session_actor.state actor |> protocol_ok in
    printf
      "adopted-current=%b unchanged-by-cancel=%b queue=%d occurrence-once=%b \
       full-archives=%d\n"
      (match result.outcome with
       | Adopted { history_id; current = Some current; admitted_content_revision = _ } ->
         P.History.Id.equal history_id entry.id && P.History.Id.equal current.id entry.id
       | Adopted { current = None; _ }
       | Pending _ | Cancelled _ | Retired _ | Unavailable _ -> false)
      (Int64.equal adopted.counters.revision after.counters.revision)
      (List.length after.conversation.deferred_user_entries)
      (Int.equal
         1
         (List.count after.conversation.canonical_history ~f:(fun current ->
            P.History.Id.equal entry.id current.id)))
      (List.length after.conversation.compaction_archives);
    [%expect
      {|adopted-current=true unchanged-by-cancel=true queue=0 occurrence-once=true full-archives=0|}])
;;

let%expect_test "legacy provenance never acquires ownership from a current writer" =
  Job_fixtures.with_actor ~prepare_state:(seed ~owner:Unknown) (fun _ _ actor writer _ ->
    let before = A.Session_actor.state actor |> protocol_ok in
    let rejected = cancel actor (request before writer entry "unknown-owner") in
    let after = A.Session_actor.state actor |> protocol_ok in
    printf
      "rejected=%b queue-kept=%b revision-kept=%b\n"
      (Result.is_error rejected)
      (List.equal
         A.Pending_input_document.equal
         before.conversation.deferred_user_entries
         after.conversation.deferred_user_entries)
      (Int64.equal before.counters.revision after.counters.revision);
    [%expect {|rejected=true queue-kept=true revision-kept=true|}])
;;

let%expect_test "rejected pending journal publishes no state event or worker" =
  let launches = ref 0 in
  Job_fixtures.with_actor
    ~prepare_state:seed
    ~reject_save:(fun transition ->
      not
        (List.is_empty
           transition.A.Session_transition.state.conversation.compaction_archives))
    (fun _ _ actor writer backend ->
       let worker =
         A.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
           Int.incr launches;
           Completed
             { final_history = input.history
             ; runtime_requests = []
             ; moderator_snapshot = None
             })
       in
       A.Session_actor.set_operation_worker actor (Some worker) |> protocol_ok;
       let before = A.Session_actor.state actor |> protocol_ok in
       let events = A.Memory_backend.events_after backend 0L |> protocol_ok in
       let rejected = cancel actor (request before writer entry "journal-reject") in
       let after = A.Session_actor.state actor |> protocol_ok in
       printf
         "rejected=%b exact-queue=%b revision-kept=%b event-count-kept=%b archives=%d \
          launches=%d\n"
         (Result.is_error rejected)
         (List.equal
            A.Pending_input_document.equal
            before.conversation.deferred_user_entries
            after.conversation.deferred_user_entries)
         (Int64.equal before.counters.revision after.counters.revision)
         (Int.equal
            (List.length events)
            (List.length (A.Memory_backend.events_after backend 0L |> protocol_ok)))
         (List.length after.conversation.compaction_archives)
         !launches;
       [%expect
         {|rejected=true exact-queue=true revision-kept=true event-count-kept=true archives=0 launches=0|}])
;;

let join promise =
  match Eio.Promise.await promise with
  | Ok () -> ()
  | Error error -> raise error
;;

let%expect_test "cancel and adoption racing through mailbox have one durable winner" =
  List.iter [ true; false ] ~f:(fun cancel_first ->
    let armed = ref false in
    let committed, committed_u = Eio.Promise.create () in
    let release, release_u = Eio.Promise.create () in
    Job_fixtures.with_actor
      ~prepare_state:seed
      ~state_committed:(fun state _ ->
        if
          !armed && List.is_empty state.A.Session_state.conversation.deferred_user_entries
        then (
          armed := false;
          Eio.Promise.resolve committed_u ();
          Eio.Promise.await release))
      (fun _ sw actor writer _ ->
         let before = A.Session_actor.state actor |> protocol_ok in
         let target = request before writer entry "race" in
         let run_cancel () = cancel actor target |> protocol_ok in
         let run_adopt () = A.Session_actor.adopt_deferred actor |> protocol_ok in
         armed := true;
         let first =
           Eio.Fiber.fork_promise ~sw (fun () ->
             if cancel_first then run_cancel () |> ignore else run_adopt () |> ignore)
         in
         Eio.Promise.await committed;
         let second =
           Eio.Fiber.fork_promise ~sw (fun () ->
             if cancel_first then run_adopt () |> ignore else run_cancel () |> ignore)
         in
         Eio.Fiber.yield ();
         Eio.Promise.resolve release_u ();
         join first;
         join second;
         let after = A.Session_actor.state actor |> protocol_ok in
         let outcome =
           A.Pending_inspection.lookup after ~history_id:entry.id ~project |> protocol_ok
         in
         printf
           "%s winner=%s pending=%d canonical-occurrences=%d archives=%d\n"
           (if cancel_first then "cancel-first" else "adopt-first")
           (match outcome with
            | Cancelled _ -> "cancelled"
            | Adopted _ -> "adopted"
            | Pending _ -> "pending"
            | Retired _ -> "retired"
            | Unavailable _ -> "unavailable")
           (List.length after.conversation.deferred_user_entries)
           (List.count after.conversation.canonical_history ~f:(fun current ->
              P.History.Id.equal current.id entry.id))
           (List.length after.conversation.compaction_archives)));
  [%expect
    {|cancel-first winner=cancelled pending=0 canonical-occurrences=0 archives=1
adopt-first winner=adopted pending=0 canonical-occurrences=1 archives=0|}]
;;

let%expect_test
    "archive rejection preserves original failure before journal and publication"
  =
  let journal_attempts = ref 0 in
  let failure =
    P.Error.create
      Persistence_error
      ~message:"pending archive rejected"
      ~retryable:false
      ()
  in
  Job_fixtures.with_actor
    ~prepare_state:seed
    ~archive_reference:(fun ~previous:_ ~kind:_ _ -> Error failure)
    ~reject_save:(fun _ ->
      Int.incr journal_attempts;
      false)
    (fun _ _ actor writer backend ->
       journal_attempts := 0;
       let before = A.Session_actor.state actor |> protocol_ok in
       let events_before =
         A.Memory_backend.events_after backend 0L |> protocol_ok |> List.length
       in
       let rejected = cancel actor (request before writer entry "archive-reject") in
       let after = A.Session_actor.state actor |> protocol_ok in
       printf
         "primary-error=%b journal-attempts=%d exact-queue=%b revision-kept=%b \
          events-kept=%b archives=%d\n"
         (match rejected with
          | Error actual ->
            P.Error.equal_code actual.code failure.code
            && String.equal actual.message failure.message
            && Bool.equal actual.retryable failure.retryable
            && String.equal (Jsonaf.to_string actual.data) (Jsonaf.to_string failure.data)
          | Ok _ -> false)
         !journal_attempts
         (List.equal
            A.Pending_input_document.equal
            before.conversation.deferred_user_entries
            after.conversation.deferred_user_entries)
         (Int64.equal before.counters.revision after.counters.revision)
         (Int.equal
            events_before
            (A.Memory_backend.events_after backend 0L |> protocol_ok |> List.length))
         (List.length after.conversation.compaction_archives);
       [%expect
         {|primary-error=true journal-attempts=0 exact-queue=true revision-kept=true events-kept=true archives=0|}])
;;

let%expect_test
    "actual reset retains history policy and explicitly retires pending ownership"
  =
  Job_fixtures.with_actor ~prepare_state:seed (fun _ _ actor writer backend ->
    A.Session_actor.stop actor ~attachment_id:writer.id ~mode:Graceful
    |> protocol_ok
    |> ignore;
    let before = A.Session_actor.state actor |> protocol_ok in
    A.Session_actor.reset
      actor
      ~attachment_id:writer.id
      ~expected_revision:before.counters.revision
      { keep_history = true
      ; keep_tasks = true
      ; keep_grants = true
      ; keep_labels = true
      ; workspace_instance = None
      }
    |> protocol_ok
    |> ignore;
    let after = A.Session_actor.state actor |> protocol_ok in
    let events =
      A.Memory_backend.events_after backend before.counters.event_sequence |> protocol_ok
    in
    assert (
      List.exists events ~f:(fun event ->
        match P.Event.Durable.replacement_snapshot event |> protocol_ok with
        | Some snapshot ->
          Int.equal snapshot.session.generation after.identity.generation
          && Int64.equal snapshot.revision after.counters.revision
        | None -> false));
    let outcome =
      A.Pending_inspection.lookup after ~history_id:entry.id ~project |> protocol_ok
    in
    let disposition = List.hd_exn after.conversation.pending_dispositions in
    printf
      "generation-advanced=%b queue-cleared=%b history-kept=%b truthful-retirement=%b \
       original-owner=%b archive=%d\n"
      (Int.equal after.identity.generation (before.identity.generation + 1))
      (List.is_empty after.conversation.deferred_user_entries)
      (List.equal
         P.History.equal_entry
         before.conversation.canonical_history
         after.conversation.canonical_history)
      (match outcome with
       | Retired (id, Source_reset) -> P.History.Id.equal id entry.id
       | Pending _ | Adopted _ | Cancelled _
       | Retired (_, (Source_replaced | Canonical_history_retired))
       | Unavailable _ -> false)
      (A.Pending_input_document.Owner.equal
         (Submitting_principal principal_id)
         (A.Pending_disposition_document.owner disposition))
      (List.length after.conversation.compaction_archives);
    [%expect
      {|generation-advanced=true queue-cleared=true history-kept=true truthful-retirement=true original-owner=true archive=1|}])
;;

let%expect_test "actual cancelled turn releases captured barrier while stop keeps queue" =
  let started, started_u = Eio.Promise.create () in
  let blocked, _ = Eio.Promise.create () in
  let runs = ref 0 in
  with_handoff_actor
    ~make_worker:(fun _ _ ->
      A.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
        Int.incr runs;
        Eio.Promise.resolve started_u ();
        Eio.Promise.await blocked;
        Completed
          { final_history = input.history
          ; runtime_requests = []
          ; moderator_snapshot = None
          }))
    (fun _ actor writer _ ->
       Eio.Promise.await started;
       let reserved =
         A.Session_actor.reserve_history_block actor ~count:1 |> protocol_ok
       in
       let id =
         History_entry.Id.create
           ~namespace:(P.Id.Session.to_string session_id)
           ~sequence:(Int64.to_int_exn reserved.first_sequence)
         |> Result.ok_or_failwith
       in
       let pending =
         A.History_codec.user_text ~id "after cancelled actual root"
         |> A.History_codec.to_protocol
       in
       A.Session_actor.submit_message
         actor
         ~submitting_principal:principal_id
         ~attachment_id:writer.id
         ~timing:After_current_operation
         pending
       |> protocol_ok
       |> ignore;
       A.Session_actor.stop actor ~attachment_id:writer.id ~mode:Cancel
       |> protocol_ok
       |> ignore;
       let rec stopped () =
         let state = A.Session_actor.state actor |> protocol_ok in
         match state.lifecycle.observed, state.active_operation with
         | Stopped, None -> state
         | _ ->
           Eio.Fiber.yield ();
           stopped ()
       in
       let state = stopped () in
       let input =
         List.hd_exn state.conversation.deferred_user_entries
         |> A.Pending_input_document.value
       in
       let released =
         match P.Pending_input.binding input with
         | After_root { terminal = Some proof; _ } ->
           (match
              Document_schema.Json.field
                (P.Pending_input.Terminal_proof.to_json proof)
                ~name:"outcome"
            with
            | Value (`String value) -> String.equal value "cancelled"
            | Absent | Null | Value _ -> false)
         | Safe_boundary | Await_idle | After_root { terminal = None; _ } -> false
       in
       let before_restart = A.Session_actor.adopt_deferred actor |> protocol_ok in
       ignore before_restart;
       let stopped_state = A.Session_actor.state actor |> protocol_ok in
       A.Session_actor.start actor ~attachment_id:writer.id |> protocol_ok |> ignore;
       A.Session_actor.adopt_deferred actor |> protocol_ok |> ignore;
       let after = A.Session_actor.state actor |> protocol_ok in
       printf
         "cancel-proof=%b stopped-keeps=%b restarted-adopts-once=%b pending-final=%d \
          worker-runs=%d\n"
         released
         (Int.equal 1 (List.length stopped_state.conversation.deferred_user_entries))
         (Int.equal
            1
            (List.count after.conversation.canonical_history ~f:(fun current ->
               P.History.Id.equal current.id id)))
         (List.length after.conversation.deferred_user_entries)
         !runs;
       [%expect
         {|cancel-proof=true stopped-keeps=true restarted-adopts-once=true pending-final=0 worker-runs=1|}])
;;
