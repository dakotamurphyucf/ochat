open! Core
module P = Agent_protocol
module Store = Agent_store
module S = Agent_session
module D = Document_schema

type t =
  { audit : Store.Idempotency_store.Command_audit.t
  ; transaction_sequence : int64
  ; transaction_hash : string
  ; restored : S.Session_persistence.Restored.t
  ; result : P.Session_seed.Result.t
  }

let audit t = t.audit
let transaction_sequence t = t.transaction_sequence
let transaction_hash t = t.transaction_hash
let restored t = t.restored
let result t = t.result
let session_id t = t.result.session_id
let corrupt reason = Error (Store.Store_error.Corrupt reason)
let document_error error = Store.Store_error.Document error

let created_document transaction ~limits =
  let open Result.Let_syntax in
  let%bind delta =
    S.Session_delta_document.decode transaction.Store.Transaction.delta ~limits
    |> Result.map_error ~f:document_error
  in
  match S.Session_delta_document.value delta with
  | Batch [ Created _ ] ->
    (match
       D.Json.field
         (D.Document.payload (S.Session_delta_document.document delta))
         ~name:"changes"
     with
     | Value (`Array [ change ]) ->
       (match D.Json.field change ~name:"state" with
        | Value json ->
          D.Document.inspect ~limits json |> Result.map_error ~f:document_error
        | Absent | Null -> corrupt "seed Created atom has no state document")
     | Absent | Null | Value _ -> corrupt "seed Created delta has another shape")
  | Batch _
  | Created _
  | Lifecycle_changed _
  | Metadata_changed _
  | Organization_changed _
  | Initial_start_consumed
  | Stop_epoch_changed _
  | Parent_stop_epoch_changed _
  | Workspace_changed _
  | Canonical_entries_appended _
  | Canonical_history_replaced _
  | Authoring_references_forgotten _
  | Authoring_publication_changed _
  | Initial_prompt_count_changed _
  | Deferred_entries_enqueued _
  | Deferred_entries_adopted
  | Pending_inputs_changed _
  | Active_operation_changed _
  | Automatic_turn_budget_enabled _
  | Automatic_turn_pauses_changed _
  | Attachment_added _
  | Attachment_removed _
  | Permission_changed _
  | Grant_changed _
  | Inference_target_captured _
  | Configuration_revision_changed _
  | Inference_target_changed _
  | Model_job_target_captured _
  | Model_job_recipe_target_captured _
  | Model_job_target_restored _
  | Inference_ledger_changed _
  | Job_changed _
  | Schedule_changed _
  | Invocation_changed _
  | Managed_submission_admitted _
  | Managed_submission_changed _
  | Managed_stop_admitted _
  | Invocation_reconciled _
  | Moderator_execution_changed _
  | Moderator_execution_reconciled _
  | Subscription_changed _
  | Subscription_expired _
  | Subscription_cancelled _
  | Delivery_changed _
  | Ingress_changed _
  | Delivery_committed _
  | Delivery_wake_changed _
  | Run_state_changed _
  | Moderator_changed _
  | Shell_changed _
  | History_block_reserved _
  | Compaction_generation_changed _
  | Compaction_archived _
  | History_deleted _
  | History_edited _
  | Owner_lease_generation_changed _
  | Failure_changed _
  | Halt_changed _
  | Reset_generation _ -> corrupt "seed evidence is not exactly one Created transition"
;;

let validate_seed_state state ~limits ~checkpoint =
  let open Result.Let_syntax in
  let state : S.Session_state.t = state in
  checkpoint ();
  let%bind () =
    match
      ( state.spec.protocol.prompt
      , state.spec.protocol.workspace
      , state.spec.delegation
      , Inference.Selection.view state.spec.inference_target )
    with
    | Catalog _, Configured _, None, Captured _
      when not state.spec.protocol.start_immediately -> Ok ()
    | ( (Catalog _ | Local_path _ | Generated _)
      , (Configured _ | Current | Local_path _)
      , _
      , _ ) -> corrupt "seed Created state is not an authored captured stopped root"
  in
  let%bind _ =
    P.Session_seed.Transcript.create
      state.conversation.canonical_history
      ~next_history_sequence:state.conversation.next_history_sequence
      ~limits
      ~max_entries:(D.Limits.max_nodes limits)
      ~max_work_steps:Int.max_value
      ~checkpoint
    |> Result.map_error ~f:(fun error -> Store.Store_error.Corrupt error.P.Error.message)
  in
  let expected =
    S.Session_state.create
      ~identity:state.identity
      ~spec:state.spec
      ~initial_history:state.conversation.canonical_history
  in
  let expected =
    { expected with
      runtime_initialization = Pending { fresh_history = false }
    ; conversation =
        { expected.conversation with
          initial_prompt_entry_count = 0
        ; next_history_sequence = state.conversation.next_history_sequence
        ; reserved_history_through = state.conversation.next_history_sequence
        }
    }
  in
  let%bind equal =
    S.Session_state_document.equal_values state ~limits expected
    |> Result.map_error ~f:document_error
  in
  checkpoint ();
  if equal && state.identity.generation = 0
  then Ok ()
  else corrupt "seed Created state contains prior state or activation"
;;

let of_created transaction ~limits ~checkpoint =
  let open Result.Let_syntax in
  checkpoint ();
  let%bind () = Store.Transaction.validate transaction in
  let%bind () = S.Session_persistence.validate_transaction ~limits transaction in
  let%bind () =
    if
      transaction.generation = 0
      && Int64.equal transaction.transaction_sequence 1L
      && Int64.equal transaction.session_revision 1L
      && Option.is_none transaction.previous_transaction_hash
      && Option.equal Int64.equal transaction.first_event_sequence (Some 1L)
      && Option.equal Int64.equal transaction.last_event_sequence (Some 1L)
    then Ok ()
    else corrupt "seed evidence is not the initial transaction"
  in
  let%bind audit =
    match transaction.command_audit with
    | None -> corrupt "seed evidence has no command audit"
    | Some document -> Store.Idempotency_store.Command_audit.decode document
  in
  let%bind () =
    if
      String.equal audit.key.method_name "session.seed"
      && audit.protected_record
      && Option.is_none audit.key.session_id
    then Ok ()
    else corrupt "seed evidence does not contain the original protected command"
  in
  let%bind document = created_document transaction ~limits in
  let%bind state_document =
    S.Session_state_document.decode document ~limits |> Result.map_error ~f:document_error
  in
  let state = S.Session_state_document.value state_document in
  let%bind () =
    if
      Option.equal
        P.Id.Principal.equal
        state.identity.creating_principal
        (Some audit.key.principal_id)
    then Ok ()
    else corrupt "seed Created principal differs from its original command"
  in
  let%bind () = validate_seed_state state ~limits ~checkpoint in
  let%bind events = S.Session_persistence.durable_events ~limits transaction in
  let%bind () =
    match events with
    | [ event ] ->
      (match P.Event.Durable.Payload.of_json ~kind:event.kind event.payload with
       | Ok (Session_created summary) ->
         if
           Jsonaf.exactly_equal
             (P.Session.to_json summary)
             (P.Session.to_json (S.Session_state.summary state))
         then Ok ()
         else corrupt "seed Created event differs from its initial state"
       | Ok _ | Error _ -> corrupt "seed evidence has another event payload")
    | [] | _ :: _ -> corrupt "seed evidence has another event inventory"
  in
  let%bind state_document =
    S.Session_persistence.apply_document state_document ~limits transaction
  in
  checkpoint ();
  Ok
    { audit
    ; transaction_sequence = transaction.transaction_sequence
    ; transaction_hash = Store.Transaction.hash transaction
    ; restored = S.Session_persistence.Restored.of_state_document state_document
    ; result = { P.Session_seed.Result.session_id = state.identity.session_id }
    }
;;

let validate_receipt t (record : Store.Idempotency_store.record) =
  if
    Store.Idempotency_store.Key.compare record.key t.audit.key <> 0
    || (not (String.equal record.request_digest t.audit.request_digest))
    || (not (Store.Idempotency_store.equal_retention record.retention Protected))
    || not
         (Option.value_map
            record.accepted_transaction_sequence
            ~default:true
            ~f:(Int64.equal t.transaction_sequence))
  then corrupt "seed Created evidence conflicts with its original protected receipt"
  else (
    match record.outcome with
    | Pending -> Ok ()
    | Failure _ -> corrupt "seed creation has a terminal rejection receipt"
    | Success json ->
      if
        not
          (Option.equal
             Int64.equal
             record.accepted_transaction_sequence
             (Some t.transaction_sequence))
      then corrupt "seed success has no exact accepted transaction sequence"
      else (
        match P.Session_seed.Result.of_json json with
        | Ok result when P.Id.Session.equal result.session_id (session_id t) -> Ok ()
        | Ok _ | Error _ -> corrupt "seed creation has another terminal identity"))
;;

let of_initial_frame frame ~limits ~checkpoint =
  let open Result.Let_syntax in
  checkpoint ();
  let%bind () =
    if Store.Frame.flags frame = 0
    then Ok ()
    else corrupt "initial journal frame has another flag"
  in
  let%bind record =
    Store.Document_record.of_frame frame ~limits ~expected_digest:None
    |> Result.map_error ~f:Store.Document_fields.record_error
  in
  let%bind transaction = Store.Transaction.decode_record record ~limits in
  checkpoint ();
  match transaction.command_audit with
  | None -> Ok None
  | Some document ->
    let%bind audit = Store.Idempotency_store.Command_audit.decode document in
    if String.equal audit.key.method_name "session.seed"
    then of_created transaction ~limits ~checkpoint |> Result.map ~f:Option.some
    else Ok None
;;
