open! Core
open Fixtures
module A = Agent_session
module P = Agent_protocol
module C = A.Moderator_checkpoint
module Q = A.Moderator_queue
module S = Session.Moderator_state.Identity_snapshot
module SD = A.Session_state_document
module DD = A.Session_delta_document
module V = A.Queue_provenance_validation
module D = Document_schema
module Raw = Moderator_checkpoint_tests

let state actor = A.Session_actor.state actor |> protocol_ok
let index (state : A.Session_state.t) = Option.value_exn state.run_state
let epoch state = (A.Run_state.installation (index state)).epoch

let snapshot (state : A.Session_state.t) =
  C.decode state.moderator |> protocol_ok |> Option.value_exn
;;

let queue (state : A.Session_state.t) =
  C.queue state.moderator |> protocol_ok |> Option.value_exn
;;

let observe (snapshot : S.t) : P.Invocation.observer =
  { script_id = snapshot.script_id; source_sha256 = snapshot.script_source_hash }
;;

let assert_same expected actual =
  let encoded state =
    SD.authored state
    |> SD.encode ~limits:document_limits
    |> document_ok
    |> D.Document.json
  in
  Raw.assert_raw (encoded expected) (encoded actual)
;;

let assert_persistence_error = function
  | Error failure -> assert (P.Error.equal_code failure.P.Error.code Persistence_error)
  | Ok _ -> failwith "injected persistence rejection was not observed"
;;

let assert_protected_error = function
  | Error failure ->
    assert (P.Error.equal_code failure.P.Error.code Invalid_state);
    assert (
      String.is_substring failure.message ~substring:"checkpoint"
      || String.is_substring failure.message ~substring:"Extension_conflict")
  | Ok _ -> failwith "protected checkpoint data was retired"
;;

let with_actor f =
  let backend = ref None in
  let reject = ref false in
  let replacement = ref None in
  Run_admission_tests.with_startup_actor
    ~prepare:(fun env ->
      let manager, _, _ =
        handoff_definition
          env
          ~declare_tool:false
          ~events:"| `Session_start -> Task.pure(state) | _ -> Task.pure(state)"
      in
      replacement
      := Some
           (Chat_response.Moderator_manager.identity_snapshot manager
            |> Result.ok_or_failwith))
    ~make_persistence:(fun _ _ initial ->
      let memory = A.Memory_backend.create ~event_capacity:128 ~initial_state:initial in
      backend := Some memory;
      let persistence = A.Memory_backend.persistence memory in
      { persistence with
        commit =
          (fun ~command_audit ~previous transition ->
            if !reject
            then
              Error
                (P.Error.create
                   Persistence_error
                   ~message:"injected queue retirement failure"
                   ~retryable:true
                   ())
            else persistence.commit ~command_audit ~previous transition)
      })
    (fun actor _ _ request start _ _ _ attachment ->
       start (request "queue-retirement") |> protocol_ok |> ignore;
       f
         actor
         (Option.value_exn !backend)
         reject
         (Option.value_exn !replacement)
         attachment)
;;

let install_queue actor =
  let before = state actor in
  let source =
    A.Run_source_installation.captured
      (A.Run_state.installation (index before))
      ~generation:before.identity.generation
    |> protocol_ok
  in
  let snapshot =
    { (snapshot before) with queued_internal_events = Moderator_queue_tests.values 2 }
  in
  let metadata =
    Q.bootstrap ~source ~queue:snapshot.queued_internal_events |> protocol_ok
  in
  let moderator =
    C.update_with_queue before.moderator ~snapshot ~queue:metadata |> protocol_ok
  in
  A.Session_actor.change_moderator actor (Some moderator) |> protocol_ok |> ignore;
  state actor
;;

let replacement before (snapshot : S.t) =
  let installation =
    A.Run_source_installation.apply
      (A.Run_state.installation (index before))
      ~change:(Replace (observe snapshot))
    |> protocol_ok
  in
  let source =
    A.Run_source_installation.captured
      installation
      ~generation:before.A.Session_state.identity.generation
    |> protocol_ok
  in
  let snapshot =
    { snapshot with queued_internal_events = [ Session.Snapshot.String "new source" ] }
  in
  let metadata =
    Q.bootstrap ~source ~queue:snapshot.queued_internal_events |> protocol_ok
  in
  C.encode_with_queue snapshot ~queue:metadata |> protocol_ok
;;

let reset_options : A.Session_actor.reset_options =
  { keep_history = false
  ; keep_tasks = false
  ; keep_grants = false
  ; keep_labels = true
  ; workspace_instance = None
  }
;;

let%expect_test "actual source and reset retire only known queue lifetimes after ACK" =
  with_actor (fun actor backend reject replacement_snapshot attachment ->
    let before = install_queue actor in
    assert (
      not
        (P.Invocation.equal_observer
           (observe (snapshot before))
           (observe replacement_snapshot)));
    let incoming = replacement before replacement_snapshot in
    let original_run = List.hd_exn (A.Run_state.runs (index before)) in
    reject := true;
    A.Session_actor.change_moderator actor (Some incoming) |> assert_persistence_error;
    assert_same before (state actor);
    assert_same before (A.Memory_backend.state backend);
    reject := false;
    A.Session_actor.change_moderator actor (Some incoming) |> protocol_ok |> ignore;
    let replaced = state actor in
    assert_same replaced (A.Memory_backend.state backend);
    assert (Int64.equal (epoch replaced) (Int64.succ (epoch before)));
    assert (Int.equal replaced.identity.generation before.identity.generation);
    assert (Int64.equal (Q.head_ordinal (queue replaced)) 0L);
    Raw.assert_raw incoming (Option.value_exn replaced.moderator);
    let retired = A.Run_state.find (index replaced) original_run.id |> Option.value_exn in
    assert (P.Run.Lifecycle.equal retired.lifecycle (Terminal Interrupted));
    assert (
      List.count
        (A.Run_state.receipts (index replaced))
        ~f:(fun receipt -> P.Run_receipt.Kind.equal receipt.kind Terminal)
      = 1);
    (* An unchanged known checkpoint is a normal write, not another source rotation. *)
    A.Session_actor.change_moderator actor (Some incoming) |> protocol_ok |> ignore;
    assert (Int64.equal (epoch (state actor)) (epoch replaced));
    A.Session_actor.stop actor ~attachment_id:attachment.id ~mode:Cancel
    |> protocol_ok
    |> ignore;
    let stopped = state actor in
    reject := true;
    A.Session_actor.reset
      actor
      ~attachment_id:attachment.id
      ~expected_revision:stopped.counters.revision
      reset_options
    |> assert_persistence_error;
    assert_same stopped (state actor);
    assert_same stopped (A.Memory_backend.state backend);
    reject := false;
    A.Session_actor.reset
      actor
      ~attachment_id:attachment.id
      ~expected_revision:stopped.counters.revision
      reset_options
    |> protocol_ok
    |> ignore;
    let reset = state actor in
    assert_same reset (A.Memory_backend.state backend);
    assert (Int.equal reset.identity.generation (stopped.identity.generation + 1));
    assert (Int64.equal (epoch reset) (Int64.succ (epoch stopped)));
    assert (Option.is_none reset.moderator);
    assert (Option.is_none (A.Run_state.installation (index reset)).source);
    let archived =
      A.Memory_backend.archived_state backend ~revision:stopped.counters.revision
      |> Option.value_exn
    in
    Raw.assert_raw
      (Option.value_exn stopped.moderator)
      (Option.value_exn archived.moderator);
    (* Same original source data can be installed in the new generation, but an
       unfamiliar checkpoint field must never follow it into a later lifetime. *)
    A.Session_actor.change_moderator actor (Some (C.encode replacement_snapshot))
    |> protocol_ok
    |> ignore;
    let protected = install_queue actor in
    let raw =
      Raw.extend
        (Option.value_exn protected.moderator)
        "future_checkpoint"
        (`Number "1.00")
    in
    A.Session_actor.change_moderator actor (Some raw) |> protocol_ok |> ignore;
    let protected = state actor in
    A.Session_actor.change_moderator actor None |> assert_protected_error;
    A.Session_actor.reset
      actor
      ~attachment_id:attachment.id
      ~expected_revision:protected.counters.revision
      reset_options
    |> assert_protected_error;
    assert_same protected (state actor);
    assert_same protected (A.Memory_backend.state backend));
  print_endline
    "source replacement and reset ACKs preserve or retire exact known data; protected \
     unknowns stay put";
  [%expect
    {| source replacement and reset ACKs preserve or retire exact known data; protected unknowns stay put |}]
;;

let projected before moderator =
  let delta =
    A.Run_source_change.prepare before ~delta:(Moderator_changed moderator) ~now:timestamp
    |> protocol_ok
  in
  let transition =
    A.Session_transition.apply ~now:timestamp before ~delta ~payloads:[] |> protocol_ok
  in
  transition
;;

let carrier before =
  SD.authored before
  |> SD.encode ~limits:document_limits
  |> document_ok
  |> SD.decode ~limits:document_limits
  |> document_ok
;;

let assert_carriers before (transition : A.Session_transition.t) =
  let previous = carrier before in
  let admitted =
    A.Session_document_transition.admit_encoded
      previous
      ~delta:transition.delta
      ~next:transition.state
      ~limits:document_limits
    |> document_ok
  in
  let actual = A.Session_state_document.Admitted.state_document admitted |> SD.value in
  assert_same transition.state actual;
  let delta =
    DD.create transition.delta ~limits:document_limits ~state_document:SD.authored
    |> document_ok
  in
  let replayed =
    DD.document delta
    |> DD.decode ~limits:document_limits
    |> document_ok
    |> fun delta -> DD.apply delta ~limits:document_limits previous |> document_ok
  in
  assert (
    Option.equal
      Jsonaf.exactly_equal
      transition.state.moderator
      (SD.value replayed).moderator)
;;

let reject_native before candidate =
  assert (
    Result.is_error
      (V.validate_transition
         ~previous:(A.Session_state.queue_provenance before)
         (A.Session_state.queue_provenance candidate)))
;;

let reject_carriers ?delta before candidate =
  reject_native before candidate;
  let previous = carrier before in
  assert (
    Result.is_error (SD.with_value previous candidate |> SD.encode ~limits:document_limits));
  (* Created cases are valid standalone DATA with a forbidden transition.
     An explicit delta also exercises final admission of invalid Running state.
     Neither actual immutable delta decoding nor apply may bypass the law. *)
  let delta = Option.value delta ~default:(A.Session_delta.Created candidate) in
  let delta =
    DD.create delta ~limits:document_limits ~state_document:SD.authored |> document_ok
  in
  let delta = DD.document delta |> DD.decode ~limits:document_limits |> document_ok in
  assert (Result.is_error (DD.apply delta ~limits:document_limits previous))
;;

let with_metadata raw metadata = Raw.replace raw "queue_provenance" (Q.to_jsonaf metadata)

let running (before : A.Session_state.t) =
  let snapshot = snapshot before in
  let metadata = queue before in
  let head = List.hd_exn (Q.entries metadata) in
  let occurrence =
    P.Moderator_execution.Queue_occurrence.create
      ~source:(Q.source metadata)
      ~ordinal:(Q.Entry.ordinal head)
      ~origin:(Q.Entry.origin head)
    |> protocol_ok
  in
  P.Moderator_execution.create_queued
    ~queue_occurrence:occurrence
    { id = P.Id.Moderator_execution.of_string "mex_retirement_running" |> protocol_ok
    ; session_id = before.identity.session_id
    ; generation = before.identity.generation
    ; source = (Q.source metadata).observer
    ; operation_id = None
    ; job = None
    ; phase = Internal_event
    ; event =
        Moderator_queue_tests.Consumed_receipt.encoded_event
          (List.hd_exn snapshot.queued_internal_events)
    ; checkpoint_sha256 = C.digest snapshot
    ; created_at = timestamp
    }
  |> protocol_ok
;;

let%expect_test "queue retirement validates source history and the full new frontier" =
  with_actor (fun actor _ _ replacement_snapshot _ ->
    let before = install_queue actor in
    let raw = replacement before replacement_snapshot in
    let valid = projected before (Some raw) in
    assert_carriers before valid;
    let removed = projected before None in
    assert_carriers before removed;
    let no_rotation = { before with moderator = None } in
    reject_carriers before no_rotation;
    let twice =
      A.Run_retirement.replace
        (index removed.state)
        ~change:Remove
        ~session_revision:(Int64.succ before.counters.revision)
        ~now:timestamp
      |> protocol_ok
    in
    reject_carriers before { removed.state with run_state = Some twice };
    let skipped_generation =
      A.Session_delta.apply
        removed.state
        (Reset_generation (before.identity.generation + 2))
      |> protocol_ok
    in
    reject_carriers before skipped_generation;
    reject_native
      before
      { removed.state with
        identity = { removed.state.identity with session_id = P.Id.Session.create () }
      };
    let metadata = C.queue (Some raw) |> protocol_ok |> Option.value_exn in
    let next_snapshot = C.decode (Some raw) |> protocol_ok |> Option.value_exn in
    let foreign =
      P.Run_source.create
        ~observer:(Q.source metadata).observer
        ~generation:(before.identity.generation + 1)
        ~installation_epoch:(Q.source metadata).installation_epoch
      |> protocol_ok
    in
    let foreign =
      Q.bootstrap ~source:foreign ~queue:next_snapshot.queued_internal_events
      |> protocol_ok
    in
    reject_native before { valid.state with moderator = Some (with_metadata raw foreign) };
    (* A well-formed nonzero frontier is still not a new source's initial queue. *)
    let shifted =
      Q.advance
        metadata
        ~before:next_snapshot.queued_internal_events
        ~after:[]
        ~consume:Consume_head
        ~appended:[]
      |> protocol_ok
    in
    let shifted =
      Q.advance
        shifted
        ~before:[]
        ~after:next_snapshot.queued_internal_events
        ~consume:Keep
        ~appended:[ Q.Capture.unavailable ]
      |> protocol_ok
    in
    reject_carriers
      before
      { valid.state with moderator = Some (with_metadata raw shifted) };
    let fake =
      Q.Capture.create ~producer:(Event (P.Id.Moderator_execution.create ())) ~origin:None
      |> protocol_ok
    in
    let fake =
      Q.bootstrap ~source:(Q.source metadata) ~queue:[]
      |> protocol_ok
      |> fun metadata ->
      Q.advance
        metadata
        ~before:[]
        ~after:next_snapshot.queued_internal_events
        ~consume:Keep
        ~appended:[ fake ]
      |> protocol_ok
    in
    reject_native before { valid.state with moderator = Some (with_metadata raw fake) };
    let old_foreign =
      Q.bootstrap
        ~source:(Q.source metadata)
        ~queue:(snapshot before).queued_internal_events
      |> protocol_ok
    in
    reject_native
      { before with
        moderator = Some (with_metadata (Option.value_exn before.moderator) old_foreign)
      }
      valid.state;
    (* Imported execution receipts below are DATA-only validation fixtures. They
       grant no callback scope or live operation ownership. *)
    let executing = running before in
    let active =
      { before with moderator_executions = executing :: before.moderator_executions }
    in
    A.Session_state.validate active |> protocol_ok;
    reject_carriers
      ~delta:(Batch [ Run_state_changed (index removed.state); Moderator_changed None ])
      active
      { removed.state with moderator_executions = active.moderator_executions };
    reject_native active removed.state;
    let interrupted =
      P.Moderator_execution.interrupt executing ~reason:"recorded recovery interruption"
      |> protocol_ok
    in
    let terminal =
      { before with moderator_executions = interrupted :: before.moderator_executions }
    in
    A.Session_state.validate terminal |> protocol_ok;
    (* An unchanged truthful old terminal receipt remains valid after rotation. *)
    projected terminal None |> assert_carriers terminal);
  print_endline
    "exact rotation, generation and destination; no invented frontier or producer; \
     Running blocks and terminal history survives";
  [%expect
    {| exact rotation, generation and destination; no invented frontier or producer; Running blocks and terminal history survives |}]
;;
