open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module D = Document_schema
module Input = P.Pending_input

let occurrence sequence text =
  let id =
    History_entry.Id.create ~namespace:(P.Id.Session.to_string session_id) ~sequence
    |> Result.ok_or_failwith
  in
  A.History_codec.user_text ~id text |> A.History_codec.to_protocol
;;

let operation generation state : P.Operation.t =
  { id = operation_id
  ; generation
  ; kind = Turn User_submit
  ; state
  ; started_at = timestamp
  ; updated_at = timestamp
  }
;;

let input state sequence timing =
  let binding =
    Input.Binding.create
      timing
      ~generation:state.A.Session_state.identity.generation
      ~operation:state.active_operation
    |> protocol_ok
  in
  Input.create
    ~entry:(occurrence sequence "pending")
    ~generation:state.identity.generation
    ~binding
  |> protocol_ok
  |> A.Pending_input_document.authored
       ~owner:(Submitting_principal principal_id)
       ~limits:document_limits
  |> document_ok
;;

let ready (original : A.Session_state.t) =
  { original with
    A.Session_state.runtime_initialization = Ready
  ; lifecycle = { desired = Running; observed = Idle }
  }
;;

let prefix state boundary queue =
  let eligibility =
    A.Pending_eligibility.create state ~boundary ~runtime_admission_open:true
    |> protocol_ok
  in
  A.Pending_eligibility.eligible_prefix eligibility queue |> protocol_ok |> List.length
;;

let%expect_test "FIFO head requires actual terminal proof, even after root disappears" =
  with_actor_workspace (fun _ workspace_instance ->
    let idle =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false |> ready
    in
    let root = operation idle.identity.generation Running in
    let running = { idle with active_operation = Some root } in
    let first = input running 0 Safe_boundary in
    let barrier = input running 1 After_current_operation in
    let last = input running 2 Safe_boundary in
    let queue = [ first; barrier; last ] in
    printf
      "worker-prefix=%d absent-root-idle-prefix=%d\n"
      (prefix running (Worker root.id) queue)
      (prefix idle Idle_start [ barrier; last ]);
    let binding = Input.binding (A.Pending_input_document.value barrier) in
    let unrelated =
      { root with
        id = P.Id.Operation.of_string "op_unrelated_pending_test" |> protocol_ok
      ; state = Completed
      }
    in
    let unrelated_proof = Input.Terminal_proof.of_operation unrelated |> protocol_ok in
    printf
      "unrelated-keeps-barrier=%b\n"
      (Input.Binding.equal
         binding
         (Input.Binding.release binding unrelated_proof |> protocol_ok));
    let proof =
      Input.Terminal_proof.of_operation { root with state = Cancelled } |> protocol_ok
    in
    let released = Input.Binding.release binding proof |> protocol_ok in
    let value =
      Input.with_binding (A.Pending_input_document.value barrier) released |> protocol_ok
    in
    let barrier =
      A.Pending_input_document.with_value barrier value ~limits:document_limits
      |> document_ok
    in
    printf
      "released-idle-prefix=%d released-worker-prefix=%d\n"
      (prefix idle Idle_start [ barrier; last ])
      (prefix running (Worker root.id) [ barrier; last ]);
    let contradictory =
      { running with
        conversation =
          { running.conversation with
            deferred_user_entries = [ barrier; last ]
          ; next_history_sequence = 8L
          ; reserved_history_through = 8L
          }
      }
    in
    let restored =
      A.Session_state_document.encode
        (A.Session_state_document.authored contradictory)
        ~limits:document_limits
      |> document_ok
      |> A.Session_state_document.decode ~limits:document_limits
      |> document_ok
      |> A.Session_state_document.value
    in
    let successor = { root with id = unrelated.id } in
    printf
      "restored-same-root-prefix=%d next-worker-prefix=%d\n"
      (prefix restored (Worker root.id) restored.conversation.deferred_user_entries)
      (prefix
         { restored with active_operation = Some successor }
         (Worker successor.id)
         restored.conversation.deferred_user_entries);
    printf
      "live-operation-is-not-proof=%b foreign-worker-is-error=%b\n"
      (Result.is_error (Input.Terminal_proof.of_operation root))
      (Result.is_error
         (A.Pending_eligibility.create
            running
            ~boundary:(Worker unrelated.id)
            ~runtime_admission_open:true));
    [%expect
      {|worker-prefix=1 absent-root-idle-prefix=0
unrelated-keeps-barrier=true
released-idle-prefix=2 released-worker-prefix=0
restored-same-root-prefix=0 next-worker-prefix=2
live-operation-is-not-proof=true foreign-worker-is-error=true|}])
;;

let%expect_test
    "adoption splits exact entry and wrapper custody without full history archive"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let state =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false |> ready
    in
    let pending = input state 0 Safe_boundary in
    let raw =
      A.Pending_input_document.to_jsonaf pending ~limits:document_limits |> document_ok
    in
    let raw =
      match raw with
      | `Object fields ->
        `Object
          (("future-wrapper", `Object [ "null", `Null; "number", `Number "1.00" ])
           :: fields)
      | _ -> assert false
    in
    let pending =
      A.Pending_input_document.of_jsonaf raw ~limits:document_limits |> document_ok
    in
    let state =
      { state with
        conversation = { state.conversation with deferred_user_entries = [ pending ] }
      }
    in
    let retention =
      A.Pending_disposition.Retention.create ~max_records:1 |> protocol_ok
    in
    let plan =
      A.Pending_plan.prepare
        state
        ~expected_pending_revision:Input.Revision.zero
        ~change:(Adopt { boundary = Idle_start; runtime_admission_open = true })
        ~limits:document_limits
        ~retention
      |> protocol_ok
    in
    let next = A.Pending_plan.apply plan state |> protocol_ok in
    let disposition = List.hd_exn next.conversation.pending_dispositions in
    let encoded =
      A.Pending_disposition_document.to_jsonaf disposition ~limits:document_limits
      |> document_ok
    in
    let restarted =
      A.Pending_disposition_document.of_jsonaf encoded ~limits:document_limits
      |> document_ok
    in
    printf
      "pending=%d canonical=%d revision=%Ld archive=%b exact-restart=%b\n"
      (List.length next.conversation.deferred_user_entries)
      (List.length next.conversation.canonical_history)
      (Input.Revision.to_int64 next.conversation.pending_revision)
      (A.Pending_plan.requires_archive plan)
      (A.Pending_disposition_document.equal disposition restarted);
    let custody =
      match D.Json.field encoded ~name:"custody" with
      | Value custody -> custody
      | _ -> assert false
    in
    printf
      "wrapper-custody=%b entry-not-duplicated=%b public-hides-custody=%b\n"
      (match D.Json.field custody ~name:"future-wrapper" with
       | Value _ -> true
       | _ -> false)
      (match D.Json.field custody ~name:"entry" with
       | Absent -> true
       | _ -> false)
      (match
         D.Json.field
           (A.Pending_disposition.to_json
              (A.Pending_disposition_document.value disposition))
           ~name:"custody"
       with
       | Absent -> true
       | _ -> false);
    [%expect
      {|pending=0 canonical=1 revision=1 archive=false exact-restart=true
wrapper-custody=true entry-not-duplicated=true public-hides-custody=true|}])
;;

let%expect_test "pending ownership survives custody independently of receipt TTL" =
  let module Owner = A.Pending_input_document.Owner in
  let other = P.Id.Principal.of_string "pri_pending_other" |> protocol_ok in
  let owner = Owner.Submitting_principal principal_id in
  let roundtrip = Owner.of_json (Owner.to_json owner) |> protocol_ok in
  printf
    "owner-roundtrip=%b matching=%b foreign=%b legacy=%b orchestration=%b\n"
    (Owner.equal owner roundtrip)
    (Result.is_ok (Owner.authorize owner ~principal:principal_id))
    (Result.is_error (Owner.authorize owner ~principal:other))
    (Result.is_error (Owner.authorize Unknown ~principal:principal_id))
    (Result.is_error (Owner.authorize Host_internal ~principal:principal_id));
  [%expect
    {|owner-roundtrip=true matching=true foreign=true legacy=true orchestration=true|}]
;;

let%expect_test "pending CAS and content CAS are independent of streaming revisions" =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false |> ready
    in
    let document = input initial 0 Safe_boundary in
    let entry = A.Pending_input_document.entry document in
    let state =
      { initial with
        counters = { initial.counters with revision = 42L }
      ; conversation = { initial.conversation with deferred_user_entries = [ document ] }
      }
    in
    let retention =
      A.Pending_disposition.Retention.create ~max_records:2 |> protocol_ok
    in
    let prepare state revision change =
      A.Pending_plan.prepare
        state
        ~expected_pending_revision:revision
        ~change
        ~retention
        ~limits:document_limits
    in
    let replacement =
      prepare
        state
        Input.Revision.zero
        (Replace_text
           { history_id = entry.id
           ; expected_content_revision = entry.content_revision
           ; text = "changed"
           })
      |> protocol_ok
    in
    let changed = A.Pending_plan.apply replacement state |> protocol_ok in
    let document = List.hd_exn changed.conversation.deferred_user_entries in
    let entry_changed = A.Pending_input_document.entry document in
    printf
      "pending=%Ld content=%Ld stream=%Ld stable=%b owner=%b archived=%b\n"
      (Input.Revision.to_int64 changed.conversation.pending_revision)
      (P.History.Content_revision.to_int64 entry_changed.content_revision)
      changed.counters.revision
      (P.History.Id.equal entry.id entry_changed.id)
      (A.Pending_input_document.Owner.equal
         (A.Pending_input_document.owner document)
         (Submitting_principal principal_id))
      (A.Pending_plan.requires_archive replacement);
    let cancel revision =
      A.Pending_plan.Change.Cancel
        { history_id = entry.id; expected_content_revision = revision }
    in
    printf
      "stale-pending=%b stale-content=%b\n"
      (Result.is_error
         (prepare changed Input.Revision.zero (cancel entry_changed.content_revision)))
      (Result.is_error
         (prepare
            changed
            changed.conversation.pending_revision
            (cancel entry.content_revision)));
    let cancelled =
      prepare
        changed
        changed.conversation.pending_revision
        (cancel entry_changed.content_revision)
      |> protocol_ok
    in
    let disposition = List.hd_exn (A.Pending_plan.dispositions cancelled) in
    printf
      "cancel-owner=%b cancel-archive=%b\n"
      (A.Pending_input_document.Owner.equal
         (A.Pending_disposition_document.owner disposition)
         (Submitting_principal principal_id))
      (A.Pending_plan.requires_archive cancelled);
    [%expect
      {|pending=1 content=1 stream=42 stable=true owner=true archived=true
stale-pending=true stale-content=true
cancel-owner=true cancel-archive=true|}])
;;

let%expect_test
    "bounded retention returns exact private records, empty adoption does not overflow"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false |> ready
    in
    let old =
      A.Pending_disposition.create
        ~history_id:(occurrence 1 "old").id
        ~generation:initial.identity.generation
        ~pending_revision:Input.Revision.zero
        ~outcome:Cancelled
      |> protocol_ok
      |> A.Pending_disposition_document.authored ~limits:document_limits
      |> document_ok
    in
    let state =
      { initial with
        conversation =
          { initial.conversation with
            deferred_user_entries = [ input initial 0 Safe_boundary ]
          ; pending_dispositions = [ old ]
          }
      }
    in
    let retention =
      A.Pending_disposition.Retention.create ~max_records:1 |> protocol_ok
    in
    let plan =
      A.Pending_plan.prepare
        state
        ~expected_pending_revision:Input.Revision.zero
        ~change:(Adopt { boundary = Idle_start; runtime_admission_open = true })
        ~retention
        ~limits:document_limits
      |> protocol_ok
    in
    printf
      "retained=%d expired-exact=%b\n"
      (List.length (A.Pending_plan.dispositions plan))
      (List.equal
         A.Pending_disposition_document.equal
         [ old ]
         (A.Pending_plan.expired_dispositions plan));
    let maximum = Input.Revision.of_int64 Int64.max_value |> protocol_ok in
    let state =
      { initial with
        conversation = { initial.conversation with pending_revision = maximum }
      }
    in
    let noop =
      A.Pending_plan.prepare
        state
        ~expected_pending_revision:maximum
        ~change:(Adopt { boundary = Idle_start; runtime_admission_open = true })
        ~retention
        ~limits:document_limits
      |> protocol_ok
    in
    printf
      "empty-max-revision-unchanged=%b\n"
      (Input.Revision.equal maximum (A.Pending_plan.revision noop));
    [%expect
      {|retained=1 expired-exact=true
empty-max-revision-unchanged=true|}])
;;

let%expect_test
    "public pending projections retain redaction and truthful unavailable outcomes"
  =
  let entry = occurrence 0 "private text" in
  let projected =
    P.Public_history.redacted
      entry.id
      ~provenance:Canonical
      (P.Public_history.Redaction.create ~disclosed_header:None)
    |> protocol_ok
  in
  let item =
    P.Pending_query.Item.create
      ~history:projected
      ~generation:0
      ~binding:Agent_protocol.Pending_input.Binding.safe_boundary
    |> protocol_ok
  in
  let view =
    P.Pending_query.View.create
      ~pending_revision:Input.Revision.zero
      ~page:{ P.Page.items = [ item ]; next_cursor = None }
    |> protocol_ok
  in
  let encoded = P.Pending_query.View.to_json view |> Jsonaf.to_string in
  printf
    "no-private-text=%b no-owner=%b duplicate-page-rejected=%b\n"
    (not (String.is_substring encoded ~substring:"private text"))
    (not (String.is_substring encoded ~substring:"owner"))
    (Result.is_error
       (P.Pending_query.View.create
          ~pending_revision:Input.Revision.zero
          ~page:{ P.Page.items = [ item; item ]; next_cursor = None }));
  let unavailable = P.Pending_query.Outcome.unavailable entry.id in
  let adopted =
    P.Pending_query.Outcome.adopted
      ~history_id:entry.id
      ~admitted_content_revision:entry.content_revision
      ~current:None
    |> protocol_ok
  in
  let kind value =
    match D.Json.field (P.Pending_query.Outcome.to_json value) ~name:"kind" with
    | Value (`String kind) -> kind
    | Absent | Null | Value _ -> assert false
  in
  printf
    "expired=%s retained-adoption-without-current=%s\n"
    (kind unavailable)
    (kind adopted);
  [%expect
    {|no-private-text=true no-owner=true duplicate-page-rejected=true
expired=unavailable retained-adoption-without-current=adopted|}]
;;

let%expect_test
    "actual worker consumption honors FIFO and terminal release before next turn"
  =
  let runs = ref 0 in
  let started, started_u = Eio.Promise.create () in
  let consume_gate, consume_u = Eio.Promise.create () in
  let consumed, consumed_u = Eio.Promise.create () in
  let complete_gate, complete_u = Eio.Promise.create () in
  with_handoff_actor
    ~make_worker:(fun _ _ ->
      A.Operation_worker.create ~run:(fun ~sw:_ ~input capabilities ->
        Int.incr runs;
        let entries =
          if Int.equal !runs 1
          then (
            Eio.Promise.resolve started_u ();
            Eio.Promise.await consume_gate;
            let entries = capabilities.consume_deferred () |> protocol_ok in
            Eio.Promise.resolve consumed_u entries;
            Eio.Promise.await complete_gate;
            entries)
          else []
        in
        Completed
          { final_history = input.history @ entries
          ; moderator_snapshot = None
          ; runtime_requests = []
          }))
    (fun _ actor writer _ ->
       Eio.Promise.await started;
       let reserved =
         A.Session_actor.reserve_history_block actor ~count:3 |> protocol_ok
       in
       let first_sequence = Int64.to_int_exn reserved.first_sequence in
       let first = occurrence first_sequence "safe"
       and barrier = occurrence (first_sequence + 1) "after root"
       and last = occurrence (first_sequence + 2) "later safe" in
       let submit timing entry =
         A.Session_actor.submit_message
           actor
           ~submitting_principal:principal_id
           ~attachment_id:writer.id
           ~timing
           entry
         |> protocol_ok
         |> ignore
       in
       submit Safe_boundary first;
       submit After_current_operation barrier;
       submit Safe_boundary last;
       Eio.Promise.resolve consume_u ();
       let entries = Eio.Promise.await consumed in
       printf
         "actual-consumed=%d first-only=%b\n"
         (List.length entries)
         (List.equal
            History_entry.Id.equal
            [ first.id ]
            (List.map entries ~f:History_entry.id));
       let current = A.Session_actor.state actor |> protocol_ok in
       printf
         "pending-after-consumption=%d active-root=%b\n"
         (List.length current.conversation.deferred_user_entries)
         (Option.is_some current.active_operation);
       Eio.Promise.resolve complete_u ();
       let terminal = await_idle actor in
       printf
         "terminal-keeps-pending=%d\n"
         (List.length terminal.conversation.deferred_user_entries);
       let started_next =
         A.Session_actor.apply_moderator_follow_up actor |> protocol_ok
       in
       let finished = await_idle actor in
       printf
         "next-started=%b total-runs=%d pending-final=%d occurrences-once=%b\n"
         started_next
         !runs
         (List.length finished.conversation.deferred_user_entries)
         (List.for_all [ first; barrier; last ] ~f:(fun original ->
            Int.equal
              1
              (List.count finished.conversation.canonical_history ~f:(fun current ->
                 P.History.Id.equal original.id current.id))));
       [%expect
         {|actual-consumed=1 first-only=true
pending-after-consumption=2 active-root=true
terminal-keeps-pending=2
next-started=true total-runs=2 pending-final=0 occurrences-once=true|}])
;;
