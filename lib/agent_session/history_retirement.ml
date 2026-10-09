open! Core
module D = Document_schema
module S = Session_state
module P = Agent_protocol

type plan =
  | Edit of History_edit.t
  | Delete of History_deletion.t

let canonical_history = function
  | Edit plan -> History_edit.canonical_history plan
  | Delete plan -> History_deletion.canonical_history plan
;;

let initial_prompt_entry_count = function
  | Edit plan -> History_edit.initial_prompt_entry_count plan
  | Delete plan -> History_deletion.initial_prompt_entry_count plan
;;

let validate_basis plan state =
  match plan with
  | Edit plan -> History_edit.validate_basis plan state
  | Delete plan -> History_deletion.validate_basis plan state
;;

let archive_kind = function
  | Edit _ -> S.Compaction_archive.Edit
  | Delete _ -> S.Compaction_archive.Delete
;;

type t =
  { previous : Session_state_document.t
  ; edit : plan
  ; archive : S.Compaction_archive.t
  }

let invalid reason = D.Error.Invalid_field { path = [ "history_edit" ]; reason }

let create_plan previous ~edit ~(archive : S.Compaction_archive.t) ~limits =
  let open Result.Let_syntax in
  let state = Session_state_document.value previous in
  let%bind () = validate_basis edit state |> Persistence_codec.document_result in
  let%bind document = Compaction_archive.archive_document previous ~limits in
  let%bind () =
    if
      (not (S.Compaction_archive.equal_kind archive.kind (archive_kind edit)))
      || (not (Int64.equal archive.revision state.counters.revision))
      || (not
            (String.equal
               archive.sha256
               (Agent_store.Document_record.digest (D.Document.to_string document))))
      || not (List.is_empty archive.invocation_dispositions)
    then
      Error
        (invalid "retirement archive does not bind the exact previous admitted document")
    else Ok ()
  in
  Ok { previous; edit; archive }
;;

let apply t ~next ~limits =
  let open Result.Let_syntax in
  let previous = Session_state_document.value t.previous in
  let old = previous.conversation
  and current = next.S.conversation in
  let%bind () =
    if
      (not (P.Id.Session.equal previous.identity.session_id next.identity.session_id))
      || (not (Int.equal previous.identity.generation next.identity.generation))
      || (not
            (List.equal
               P.History.equal_entry
               current.canonical_history
               (canonical_history t.edit)))
      || (not
            (List.equal
               Pending_input_document.equal
               current.deferred_user_entries
               old.deferred_user_entries))
      || (not (Int64.equal current.next_history_sequence old.next_history_sequence))
      || (not (Int64.equal current.reserved_history_through old.reserved_history_through))
      || (not
            (Int.equal
               current.initial_prompt_entry_count
               (initial_prompt_entry_count t.edit)))
      || (not (Option.equal Jsonaf.exactly_equal previous.moderator next.moderator))
      || not
           (List.exists
              current.compaction_archives
              ~f:(fun (reference : S.Compaction_archive.t) ->
                P.Id.Operation.equal reference.operation_id t.archive.operation_id
                && S.Compaction_archive.equal_kind reference.kind t.archive.kind
                && Int64.equal reference.revision t.archive.revision
                && String.equal reference.sha256 t.archive.sha256
                && List.is_empty reference.invocation_dispositions))
    then Error (invalid "candidate does not match admitted exact history retirement")
    else Ok ()
  in
  let%bind admitted =
    match t.edit with
    | Edit plan ->
      Session_state_document.retire_canonical_suffix
        t.previous
        ~retained_ids:
          (List.map (History_edit.canonical_history plan) ~f:(fun entry ->
             entry.P.History.id))
        ~limits
    | Delete plan ->
      Session_state_document.retire_canonical_deletion t.previous ~deletion:plan ~limits
  in
  let candidate = Session_state_document.with_value admitted next in
  let%map _ = Session_state_document.encode candidate ~limits in
  candidate
;;

let create previous ~edit ~archive ~limits =
  create_plan previous ~edit:(Edit edit) ~archive ~limits
;;

let admit previous ~delta ~next ~limits =
  let rec collect values = function
    | Session_delta.Batch changes -> List.fold changes ~init:values ~f:collect
    | History_edited (edit, archive) -> (`Edit edit, archive) :: values
    | History_deleted (id, archive) -> (`Delete id, archive) :: values
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
    | Owner_lease_generation_changed _
    | Failure_changed _
    | Halt_changed _
    | Reset_generation _ -> values
  in
  let open Result.Let_syntax in
  match collect [] delta with
  | [] -> Ok (Session_state_document.with_value previous next)
  | [ (edit, archive) ] ->
    let%bind plan =
      (match edit with
       | `Edit edit ->
         Result.map
           (History_edit.prepare (Session_state_document.value previous) ~edit)
           ~f:(fun plan -> Edit plan)
       | `Delete history_id ->
         Result.map
           (History_deletion.prepare (Session_state_document.value previous) ~history_id)
           ~f:(fun plan -> Delete plan))
      |> Persistence_codec.document_result
    in
    let%bind admission = create_plan previous ~edit:plan ~archive ~limits in
    apply admission ~next ~limits
  | _ -> Error (invalid "a transaction may contain only one history mutation")
;;
