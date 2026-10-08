open! Core
module P = Agent_protocol
module X = Persistence_codec
module J = P.Json_codec
module D = Document_schema
module Delta = Session_delta

type t =
  { value : Delta.t
  ; document : D.Document.t
  }

let value t = t.value
let document t = t.document

let protocol_error error =
  D.Error.Invalid_field { path = []; reason = error.P.Error.message }
;;

let shell_of_jsonaf json =
  Session.Shell_state.of_jsonaf json |> Result.map_error ~f:P.Error.invalid_request
;;

let flatten delta =
  let rec loop acc = function
    | [] -> List.rev acc
    | Delta.Batch xs :: rest -> loop acc (xs @ rest)
    | x :: rest -> loop (x :: acc) rest
  in
  loop [] [ delta ]
;;

let atom_to_jsonaf ~limits ~state_document delta =
  let open Result.Let_syntax in
  let created_to_jsonaf state =
    Session_state_document.encode (state_document state) ~limits
    |> Result.map ~f:D.Document.json
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
  in
  match delta with
  | Delta.Batch _ -> assert false
  | Created value ->
    let%map json = created_to_jsonaf value in
    `Object [ "kind", `String "created"; "state", json ]
  | Lifecycle_changed value ->
    Ok
      (`Object
          [ "kind", `String "lifecycle_changed"
          ; "lifecycle", Session_state_document.lifecycle_to_jsonaf value
          ])
  | Metadata_changed (values, revision) ->
    Ok
      (`Object
          [ "kind", `String "metadata_changed"
          ; "display_name", X.option_json X.text_json values.display_name
          ; ( "labels"
            , X.list_json
                (fun (name, value) ->
                   `Object [ "name", `String name; "value", `String value ])
                values.labels )
          ; "metadata_revision", X.int64_json revision
          ])
  | Initial_start_consumed -> Ok (`Object [ "kind", `String "initial_start_consumed" ])
  | Stop_epoch_changed value ->
    Ok (`Object [ "kind", `String "stop_epoch_changed"; "epoch", X.int64_json value ])
  | Parent_stop_epoch_changed value ->
    Ok
      (`Object
          [ "kind", `String "parent_stop_epoch_changed"; "epoch", X.int64_json value ])
  | Workspace_changed value ->
    Ok
      (`Object
          [ "kind", `String "workspace_changed"
          ; "workspace", Workspace_instance.to_jsonaf value
          ])
  | Canonical_entries_appended value ->
    Ok
      (`Object
          [ "kind", `String "canonical_entries_appended"
          ; "entries", (X.list_json P.History.entry_to_json) value
          ])
  | Canonical_history_replaced value ->
    Ok
      (`Object
          [ "kind", `String "canonical_history_replaced"
          ; "entries", (X.list_json P.History.entry_to_json) value
          ])
  | Deferred_entries_enqueued value ->
    Ok
      (`Object
          [ "kind", `String "deferred_entries_enqueued"
          ; "entries", (X.list_json P.History.entry_to_json) value
          ])
  | Authoring_references_forgotten value ->
    Ok
      (`Object
          [ "kind", `String "authoring_references_forgotten"
          ; "ids", (X.list_json P.History.Id.to_json) value
          ])
  | Authoring_publication_changed value ->
    Ok
      (`Object
          [ "kind", `String "authoring_publication_changed"
          ; "publication", Chat_response.Authoring_publication.context_to_jsonaf value
          ])
  | Initial_prompt_count_changed value ->
    Ok
      (`Object
          [ "kind", `String "initial_prompt_count_changed"
          ; "count", X.integer_json value
          ])
  | Deferred_entries_adopted ->
    Ok (`Object [ "kind", `String "deferred_entries_adopted" ])
  | Active_operation_changed value ->
    Ok
      (`Object
          [ "kind", `String "active_operation_changed"
          ; "operation", (X.option_json P.Operation.to_json) value
          ])
  | Automatic_turn_budget_enabled value ->
    Ok
      (`Object
          [ "kind", `String "automatic_turn_budget_enabled"
          ; "policy", Automatic_turn_budget.policy_to_jsonaf value
          ])
  | Automatic_turn_pauses_changed value ->
    Ok
      (`Object
          [ "kind", `String "automatic_turn_pauses_changed"
          ; "pauses", (X.list_json Automatic_turn_budget.pause_to_jsonaf) value
          ])
  | Attachment_added value ->
    Ok
      (`Object
          [ "kind", `String "attachment_added"
          ; "attachment", Session_state_document.attachment_to_jsonaf value
          ])
  | Attachment_removed value ->
    Ok
      (`Object
          [ "kind", `String "attachment_removed"
          ; "attachment_id", P.Id.Attachment.to_json value
          ])
  | Permission_changed value ->
    Ok
      (`Object
          [ "kind", `String "permission_changed"; "value", P.Permission.to_json value ])
  | Grant_changed value ->
    Ok (`Object [ "kind", `String "grant_changed"; "value", P.Grant.to_json value ])
  | Inference_target_captured target | Inference_target_changed target ->
    let%map () =
      Inference.Request.Target.validate target ~limits
      |> Result.map_error ~f:(fun error ->
        P.Error.invalid_request
          (Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error)))
    in
    `Object
      [ ( "kind"
        , `String
            (match delta with
             | Inference_target_captured _ -> "inference_target_captured"
             | _ -> "inference_target_changed") )
      ; "target", Inference.Request.Target.to_json target
      ]
  | Model_job_target_captured binding
  | Model_job_recipe_target_captured binding
  | Model_job_target_restored binding ->
    Ok
      (`Object
          [ ( "kind"
            , `String
                (match delta with
                 | Model_job_target_captured _ -> "model_job_target_captured"
                 | Model_job_recipe_target_captured _ ->
                   "model_job_recipe_target_captured"
                 | _ -> "model_job_target_restored") )
          ; "value", Model_job_target.to_json binding
          ])
  | Inference_ledger_changed value ->
    let%map document =
      Inference_ledger.to_document value
      |> Result.map_error ~f:(fun error ->
        P.Error.invalid_request
          (Sexp.to_string_hum (Inference_ledger.Error.sexp_of_t error)))
    in
    `Object
      [ "kind", `String "inference_ledger_changed"; "value", D.Document.json document ]
  | Job_changed value ->
    let%bind () =
      match value.P.Job.status with
      | Waiting_completion { completion_schema = Some `Null; _ } ->
        Error (P.Error.invalid_request "invalid captured job completion schema")
      | Waiting_completion _
      | Queued
      | Running
      | Waiting_permission _
      | Succeeded
      | Failed _
      | Cancelled
      | Interrupted _ -> Ok ()
    in
    Ok (`Object [ "kind", `String "job_changed"; "value", P.Job.to_json value ])
  | Schedule_changed value ->
    Ok
      (`Object
          [ "kind", `String "schedule_changed"
          ; "value", P.Schedule.Storage.to_json value
          ])
  | Invocation_changed value ->
    Ok
      (`Object
          [ "kind", `String "invocation_changed"
          ; "value", P.Invocation.Storage.to_json value
          ])
  | Invocation_reconciled value ->
    Ok
      (`Object
          [ "kind", `String "invocation_reconciled"
          ; "value", P.Invocation.Storage.to_json value
          ])
  | Moderator_execution_changed value ->
    Ok
      (`Object
          [ "kind", `String "moderator_execution_changed"
          ; "value", P.Moderator_execution.to_json value
          ])
  | Moderator_execution_reconciled value ->
    Ok
      (`Object
          [ "kind", `String "moderator_execution_reconciled"
          ; "value", P.Moderator_execution.to_json value
          ])
  | Subscription_changed value ->
    Ok
      (`Object
          [ "kind", `String "subscription_changed"
          ; "value", P.Subscription.Storage.to_json value
          ])
  | Subscription_expired value ->
    Ok
      (`Object
          [ "kind", `String "subscription_expired"
          ; "value", P.Subscription.Storage.to_json value
          ])
  | Subscription_cancelled value ->
    Ok
      (`Object
          [ "kind", `String "subscription_cancelled"
          ; "value", P.Subscription.Storage.to_json value
          ])
  | Delivery_changed value ->
    Ok
      (`Object
          [ "kind", `String "delivery_changed"
          ; "value", P.Delivery.Storage.to_json value
          ])
  | Delivery_wake_changed value ->
    Ok
      (`Object
          [ "kind", `String "delivery_wake_changed"
          ; "value", P.Delivery.Storage.to_json value
          ])
  | Managed_submission_admitted value ->
    Ok
      (`Object
          [ "kind", `String "managed_submission_admitted"
          ; "value", Managed_submission.to_jsonaf value
          ])
  | Managed_submission_changed value ->
    Ok
      (`Object
          [ "kind", `String "managed_submission_changed"
          ; "value", Managed_submission.to_jsonaf value
          ])
  | Managed_stop_admitted value ->
    Ok
      (`Object
          [ "kind", `String "managed_stop_admitted"
          ; "value", Managed_stop.to_jsonaf value
          ])
  | Ingress_changed value ->
    Ok
      (`Object
          [ "kind", `String "ingress_changed"; "value", External_ingress.to_jsonaf value ])
  | Moderator_changed value ->
    let%bind () =
      match value with
      | Some `Null -> Result.map (X.moderator_of_jsonaf `Null) ~f:ignore
      | None | Some _ -> Ok ()
    in
    Ok
      (`Object
          [ "kind", `String "moderator_changed"
          ; "moderator", (X.option_json Fn.id) value
          ])
  | Shell_changed value ->
    Ok
      (`Object
          [ "kind", `String "shell_changed"
          ; "shell", Session.Shell_state.to_jsonaf value
          ])
  | History_block_reserved value ->
    Ok
      (`Object
          [ "kind", `String "history_block_reserved"; "sequence", X.int64_json value ])
  | Compaction_generation_changed value ->
    Ok
      (`Object
          [ "kind", `String "compaction_generation_changed"
          ; "generation", X.integer_json value
          ])
  | Compaction_archived value ->
    Ok
      (`Object
          [ "kind", `String "compaction_archived"
          ; "archive", Session_state_document.archive_reference_to_jsonaf value
          ])
  | Owner_lease_generation_changed value ->
    Ok
      (`Object
          [ "kind", `String "owner_lease_generation_changed"
          ; "generation", X.int64_json value
          ])
  | Failure_changed value ->
    Ok
      (`Object
          [ "kind", `String "failure_changed"
          ; "failure", (X.option_json P.Error.to_json) value
          ])
  | Halt_changed value ->
    Ok
      (`Object
          [ "kind", `String "halt_changed"; "reason", (X.option_json X.text_json) value ])
  | Reset_generation value ->
    Ok
      (`Object [ "kind", `String "reset_generation"; "generation", X.integer_json value ])
  | Delivery_committed (delivery, entry) ->
    Ok
      (`Object
          [ "kind", `String "delivery_committed"
          ; "delivery", P.Delivery.Storage.to_json delivery
          ; "entry", P.History.entry_to_json entry
          ])
;;

let atom_of_jsonaf ~limits json =
  let open Result.Let_syntax in
  let created_of_jsonaf json =
    D.Document.inspect ~limits json
    |> Result.bind ~f:(Session_state_document.decode ~limits)
    |> Result.map ~f:Session_state_document.value
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
  in
  let%bind fields = X.object_ json in
  let%bind kind = X.required fields "kind" J.string in
  match kind with
  | "created" ->
    Result.map (X.required fields "state" created_of_jsonaf) ~f:(fun value ->
      Delta.Created value)
  | "lifecycle_changed" ->
    Result.map
      (X.required fields "lifecycle" Session_state_document.lifecycle_of_jsonaf)
      ~f:(fun value -> Delta.Lifecycle_changed value)
  | "metadata_changed" ->
    let%bind display_name = X.required fields "display_name" (X.nullable J.string) in
    let%bind labels =
      X.required
        fields
        "labels"
        (X.list (fun json ->
           let%bind fields = X.object_ json in
           let%bind name = X.required fields "name" J.string in
           let%map value = X.required fields "value" J.string in
           name, value))
    in
    let%bind values = P.Session_metadata.Values.create ~display_name ~labels in
    let%map revision = X.required fields "metadata_revision" X.nonnegative_int64 in
    Delta.Metadata_changed (values, revision)
  | "initial_start_consumed" -> Ok Delta.Initial_start_consumed
  | "stop_epoch_changed" ->
    Result.map (X.required fields "epoch" X.nonnegative_int64) ~f:(fun value ->
      Delta.Stop_epoch_changed value)
  | "parent_stop_epoch_changed" ->
    Result.map (X.required fields "epoch" X.nonnegative_int64) ~f:(fun value ->
      Delta.Parent_stop_epoch_changed value)
  | "workspace_changed" ->
    Result.map
      (X.required fields "workspace" Workspace_instance.of_jsonaf)
      ~f:(fun value -> Delta.Workspace_changed value)
  | "canonical_entries_appended" ->
    Result.map
      (X.required fields "entries" (X.list P.History.entry_of_json))
      ~f:(fun value -> Delta.Canonical_entries_appended value)
  | "canonical_history_replaced" ->
    Result.map
      (X.required fields "entries" (X.list P.History.entry_of_json))
      ~f:(fun value -> Delta.Canonical_history_replaced value)
  | "deferred_entries_enqueued" ->
    Result.map
      (X.required fields "entries" (X.list P.History.entry_of_json))
      ~f:(fun value -> Delta.Deferred_entries_enqueued value)
  | "authoring_references_forgotten" ->
    Result.map
      (X.required fields "ids" (X.list P.History.Id.of_json))
      ~f:(fun value -> Delta.Authoring_references_forgotten value)
  | "authoring_publication_changed" ->
    Result.map
      (X.required
         fields
         "publication"
         Chat_response.Authoring_publication.context_of_jsonaf)
      ~f:(fun value -> Delta.Authoring_publication_changed value)
  | "initial_prompt_count_changed" ->
    Result.map (X.required fields "count" X.integer) ~f:(fun value ->
      Delta.Initial_prompt_count_changed value)
  | "deferred_entries_adopted" -> Ok Delta.Deferred_entries_adopted
  | "active_operation_changed" ->
    Result.map
      (X.required fields "operation" (X.nullable P.Operation.of_json))
      ~f:(fun value -> Delta.Active_operation_changed value)
  | "automatic_turn_budget_enabled" ->
    Result.map
      (X.required fields "policy" Automatic_turn_budget.policy_of_jsonaf)
      ~f:(fun value -> Delta.Automatic_turn_budget_enabled value)
  | "automatic_turn_pauses_changed" ->
    Result.map
      (X.required fields "pauses" (X.list Automatic_turn_budget.pause_of_jsonaf))
      ~f:(fun value -> Delta.Automatic_turn_pauses_changed value)
  | "attachment_added" ->
    Result.map
      (X.required fields "attachment" Session_state_document.attachment_of_jsonaf)
      ~f:(fun value -> Delta.Attachment_added value)
  | "attachment_removed" ->
    Result.map
      (X.required fields "attachment_id" P.Id.Attachment.of_json)
      ~f:(fun value -> Delta.Attachment_removed value)
  | "permission_changed" ->
    Result.map (X.required fields "value" P.Permission.of_json) ~f:(fun value ->
      Delta.Permission_changed value)
  | "grant_changed" ->
    Result.map (X.required fields "value" P.Grant.of_json) ~f:(fun value ->
      Delta.Grant_changed value)
  | "inference_target_captured" | "inference_target_changed" ->
    let%map target =
      X.required fields "target" (fun json ->
        Inference.Request.Target.of_json json ~limits
        |> Result.map_error ~f:(fun error ->
          P.Error.invalid_request
            (Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))))
    in
    if String.equal kind "inference_target_captured"
    then Delta.Inference_target_captured target
    else Delta.Inference_target_changed target
  | "model_job_target_captured"
  | "model_job_recipe_target_captured"
  | "model_job_target_restored" ->
    let%map binding = X.required fields "value" (Model_job_target.of_json ~limits) in
    if String.equal kind "model_job_target_captured"
    then Delta.Model_job_target_captured binding
    else if String.equal kind "model_job_recipe_target_captured"
    then Delta.Model_job_recipe_target_captured binding
    else Delta.Model_job_target_restored binding
  | "inference_ledger_changed" ->
    let%bind json = X.required fields "value" X.raw in
    let%bind document =
      D.Document.inspect ~limits json
      |> Result.map_error ~f:(fun error ->
        P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
    in
    let%map ledger =
      Inference_ledger.of_document document ~limits:Inference_ledger.Limits.default
      |> Result.map_error ~f:(fun error ->
        P.Error.invalid_request
          (Sexp.to_string_hum (Inference_ledger.Error.sexp_of_t error)))
    in
    Delta.Inference_ledger_changed ledger
  | "job_changed" ->
    Result.map (X.required fields "value" P.Job.of_json) ~f:(fun value ->
      Delta.Job_changed value)
  | "schedule_changed" ->
    Result.map (X.required fields "value" P.Schedule.Storage.of_json) ~f:(fun value ->
      Delta.Schedule_changed value)
  | "invocation_changed" ->
    Result.map (X.required fields "value" P.Invocation.Storage.of_json) ~f:(fun value ->
      Delta.Invocation_changed value)
  | "invocation_reconciled" ->
    Result.map (X.required fields "value" P.Invocation.Storage.of_json) ~f:(fun value ->
      Delta.Invocation_reconciled value)
  | "moderator_execution_changed" ->
    Result.map (X.required fields "value" P.Moderator_execution.of_json) ~f:(fun value ->
      Delta.Moderator_execution_changed value)
  | "moderator_execution_reconciled" ->
    Result.map (X.required fields "value" P.Moderator_execution.of_json) ~f:(fun value ->
      Delta.Moderator_execution_reconciled value)
  | "subscription_changed" ->
    Result.map (X.required fields "value" P.Subscription.Storage.of_json) ~f:(fun value ->
      Delta.Subscription_changed value)
  | "subscription_expired" ->
    Result.map (X.required fields "value" P.Subscription.Storage.of_json) ~f:(fun value ->
      Delta.Subscription_expired value)
  | "subscription_cancelled" ->
    Result.map (X.required fields "value" P.Subscription.Storage.of_json) ~f:(fun value ->
      Delta.Subscription_cancelled value)
  | "delivery_changed" ->
    Result.map (X.required fields "value" P.Delivery.Storage.of_json) ~f:(fun value ->
      Delta.Delivery_changed value)
  | "delivery_wake_changed" ->
    Result.map (X.required fields "value" P.Delivery.Storage.of_json) ~f:(fun value ->
      Delta.Delivery_wake_changed value)
  | "managed_submission_admitted" ->
    Result.map (X.required fields "value" Managed_submission.of_jsonaf) ~f:(fun value ->
      Delta.Managed_submission_admitted value)
  | "managed_submission_changed" ->
    Result.map (X.required fields "value" Managed_submission.of_jsonaf) ~f:(fun value ->
      Delta.Managed_submission_changed value)
  | "managed_stop_admitted" ->
    Result.map (X.required fields "value" Managed_stop.of_jsonaf) ~f:(fun value ->
      Delta.Managed_stop_admitted value)
  | "ingress_changed" ->
    Result.map (X.required fields "value" External_ingress.of_jsonaf) ~f:(fun value ->
      Delta.Ingress_changed value)
  | "moderator_changed" ->
    Result.map
      (X.required fields "moderator" (X.nullable X.moderator_of_jsonaf))
      ~f:(fun value -> Delta.Moderator_changed value)
  | "shell_changed" ->
    Result.map (X.required fields "shell" shell_of_jsonaf) ~f:(fun value ->
      Delta.Shell_changed value)
  | "history_block_reserved" ->
    Result.map (X.required fields "sequence" X.nonnegative_int64) ~f:(fun value ->
      Delta.History_block_reserved value)
  | "compaction_generation_changed" ->
    Result.map (X.required fields "generation" X.integer) ~f:(fun value ->
      Delta.Compaction_generation_changed value)
  | "compaction_archived" ->
    Result.map
      (X.required fields "archive" Session_state_document.archive_reference_of_jsonaf)
      ~f:(fun value -> Delta.Compaction_archived value)
  | "owner_lease_generation_changed" ->
    Result.map (X.required fields "generation" X.nonnegative_int64) ~f:(fun value ->
      Delta.Owner_lease_generation_changed value)
  | "failure_changed" ->
    Result.map
      (X.required fields "failure" (X.nullable P.Error.of_json))
      ~f:(fun value -> Delta.Failure_changed value)
  | "halt_changed" ->
    Result.map
      (X.required fields "reason" (X.nullable J.string))
      ~f:(fun value -> Delta.Halt_changed value)
  | "reset_generation" ->
    Result.map (X.required fields "generation" X.integer) ~f:(fun value ->
      Delta.Reset_generation value)
  | "delivery_committed" ->
    let%bind delivery = X.required fields "delivery" P.Delivery.Storage.of_json in
    let%map entry = X.required fields "entry" P.History.entry_of_json in
    Delta.Delivery_committed (delivery, entry)
  | _ -> Error (P.Error.invalid_request "unknown delta change kind")
;;

let shape =
  X.shape_exn
    [ ( "changes"
      , X.array_shape_exn
          (X.tagged_shape_exn
             ~discriminator:"kind"
             [ "created", X.shape_exn [ "kind", D.Shape.value; "state", D.Shape.value ]
             ; ( "lifecycle_changed"
               , X.shape_exn
                   [ "kind", D.Shape.value; "lifecycle", Session_record_shapes.lifecycle ]
               )
             ; ( "metadata_changed"
               , X.shape_exn
                   [ "kind", D.Shape.value
                   ; "display_name", X.nullable_shape D.Shape.value
                   ; ( "labels"
                     , X.array_shape_exn
                         ~allow_empty_identity:true
                         ~identity_field:"name"
                         (X.fields_shape [ "name"; "value" ]) )
                   ; "metadata_revision", D.Shape.value
                   ] )
             ; "initial_start_consumed", X.shape_exn [ "kind", D.Shape.value ]
             ; ( "stop_epoch_changed"
               , X.shape_exn [ "kind", D.Shape.value; "epoch", D.Shape.value ] )
             ; ( "parent_stop_epoch_changed"
               , X.shape_exn [ "kind", D.Shape.value; "epoch", D.Shape.value ] )
             ; ( "workspace_changed"
               , X.shape_exn
                   [ "kind", D.Shape.value; "workspace", Workspace_instance.shape ] )
             ; ( "canonical_entries_appended"
               , X.shape_exn
                   [ "kind", D.Shape.value
                   ; ( "entries"
                     , X.array_shape_exn
                         ~identity_field:"id"
                         Session_record_shapes.history_entry )
                   ] )
             ; ( "canonical_history_replaced"
               , X.shape_exn
                   [ "kind", D.Shape.value
                   ; ( "entries"
                     , X.array_shape_exn
                         ~identity_field:"id"
                         Session_record_shapes.history_entry )
                   ] )
             ; ( "deferred_entries_enqueued"
               , X.shape_exn
                   [ "kind", D.Shape.value
                   ; ( "entries"
                     , X.array_shape_exn
                         ~identity_field:"id"
                         Session_record_shapes.history_entry )
                   ] )
             ; ( "authoring_references_forgotten"
               , X.shape_exn [ "kind", D.Shape.value; "ids", D.Shape.value ] )
             ; ( "authoring_publication_changed"
               , X.shape_exn
                   [ "kind", D.Shape.value
                   ; "publication", Chat_response.Authoring_publication.context_shape
                   ] )
             ; ( "initial_prompt_count_changed"
               , X.shape_exn [ "kind", D.Shape.value; "count", D.Shape.value ] )
             ; "deferred_entries_adopted", X.shape_exn [ "kind", D.Shape.value ]
             ; ( "active_operation_changed"
               , X.shape_exn
                   [ "kind", D.Shape.value
                   ; "operation", D.Shape.nullable Session_record_shapes.operation
                   ] )
             ; ( "automatic_turn_budget_enabled"
               , X.shape_exn
                   [ "kind", D.Shape.value; "policy", Automatic_turn_budget.policy_shape ]
               )
             ; ( "automatic_turn_pauses_changed"
               , X.shape_exn [ "kind", D.Shape.value; "pauses", D.Shape.value ] )
             ; ( "attachment_added"
               , X.shape_exn
                   [ "kind", D.Shape.value
                   ; "attachment", Session_state_document.attachment_shape
                   ] )
             ; ( "attachment_removed"
               , X.shape_exn [ "kind", D.Shape.value; "attachment_id", D.Shape.value ] )
             ; ( "permission_changed"
               , X.shape_exn
                   [ "kind", D.Shape.value; "value", Session_record_shapes.permission ] )
             ; ( "grant_changed"
               , X.shape_exn
                   [ "kind", D.Shape.value; "value", Session_record_shapes.grant ] )
             ; ( "inference_target_captured"
               , X.shape_exn [ "kind", D.Shape.value; "target", D.Shape.value ] )
             ; ( "inference_target_changed"
               , X.shape_exn [ "kind", D.Shape.value; "target", D.Shape.value ] )
             ; ( "model_job_target_captured"
               , X.shape_exn [ "kind", D.Shape.value; "value", Model_job_target.shape ] )
             ; ( "model_job_recipe_target_captured"
               , X.shape_exn [ "kind", D.Shape.value; "value", Model_job_target.shape ] )
             ; ( "model_job_target_restored"
               , X.shape_exn [ "kind", D.Shape.value; "value", Model_job_target.shape ] )
             ; ( "inference_ledger_changed"
               , X.shape_exn [ "kind", D.Shape.value; "value", D.Shape.value ] )
             ; ( "job_changed"
               , X.shape_exn [ "kind", D.Shape.value; "value", Session_record_shapes.job ]
               )
             ; ( "schedule_changed"
               , X.shape_exn
                   [ "kind", D.Shape.value; "value", Session_record_shapes.schedule ] )
             ; ( "invocation_changed"
               , X.shape_exn
                   [ "kind", D.Shape.value; "value", Session_record_shapes.invocation ] )
             ; ( "invocation_reconciled"
               , X.shape_exn
                   [ "kind", D.Shape.value; "value", Session_record_shapes.invocation ] )
             ; ( "moderator_execution_changed"
               , X.shape_exn
                   [ "kind", D.Shape.value
                   ; "value", Session_record_shapes.moderator_execution
                   ] )
             ; ( "moderator_execution_reconciled"
               , X.shape_exn
                   [ "kind", D.Shape.value
                   ; "value", Session_record_shapes.moderator_execution
                   ] )
             ; ( "subscription_changed"
               , X.shape_exn
                   [ "kind", D.Shape.value; "value", Session_record_shapes.subscription ]
               )
             ; ( "subscription_expired"
               , X.shape_exn
                   [ "kind", D.Shape.value; "value", Session_record_shapes.subscription ]
               )
             ; ( "subscription_cancelled"
               , X.shape_exn
                   [ "kind", D.Shape.value; "value", Session_record_shapes.subscription ]
               )
             ; ( "delivery_changed"
               , X.shape_exn
                   [ "kind", D.Shape.value; "value", Session_record_shapes.delivery ] )
             ; ( "delivery_wake_changed"
               , X.shape_exn
                   [ "kind", D.Shape.value; "value", Session_record_shapes.delivery ] )
             ; ( "managed_submission_admitted"
               , X.shape_exn [ "kind", D.Shape.value; "value", Managed_submission.shape ]
               )
             ; ( "managed_submission_changed"
               , X.shape_exn [ "kind", D.Shape.value; "value", Managed_submission.shape ]
               )
             ; ( "managed_stop_admitted"
               , X.shape_exn [ "kind", D.Shape.value; "value", Managed_stop.shape ] )
             ; ( "ingress_changed"
               , X.shape_exn [ "kind", D.Shape.value; "value", External_ingress.shape ] )
             ; ( "moderator_changed"
               , X.shape_exn
                   [ "kind", D.Shape.value
                   ; "moderator", D.Shape.nullable Moderator_checkpoint.shape
                   ] )
             ; ( "shell_changed"
               , X.shape_exn [ "kind", D.Shape.value; "shell", Session.Shell_state.shape ]
               )
             ; ( "history_block_reserved"
               , X.shape_exn [ "kind", D.Shape.value; "sequence", D.Shape.value ] )
             ; ( "compaction_generation_changed"
               , X.shape_exn [ "kind", D.Shape.value; "generation", D.Shape.value ] )
             ; ( "compaction_archived"
               , X.shape_exn
                   [ "kind", D.Shape.value
                   ; "archive", Session_state_document.archive_reference_shape
                   ] )
             ; ( "owner_lease_generation_changed"
               , X.shape_exn [ "kind", D.Shape.value; "generation", D.Shape.value ] )
             ; ( "failure_changed"
               , X.shape_exn
                   [ "kind", D.Shape.value
                   ; "failure", D.Shape.nullable Session_record_shapes.error
                   ] )
             ; ( "halt_changed"
               , X.shape_exn [ "kind", D.Shape.value; "reason", D.Shape.value ] )
             ; ( "reset_generation"
               , X.shape_exn [ "kind", D.Shape.value; "generation", D.Shape.value ] )
             ; ( "delivery_committed"
               , X.shape_exn
                   [ "kind", D.Shape.value
                   ; "delivery", Session_record_shapes.delivery
                   ; "entry", Session_record_shapes.history_entry
                   ] )
             ]) )
    ]
;;

let decode_payload ~limits json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%map changes = X.required fields "changes" (X.list (atom_of_jsonaf ~limits)) in
  Delta.Batch changes
;;

let upgrade document ~limits =
  let module F = Agent_store.Document_fields in
  let open Result.Let_syntax in
  let%bind step =
    D.Conversion.Step.of_function ~kind:"session.delta" ~from_version:1 ~f:(fun payload ->
      let%bind changes = F.required payload "changes" F.array in
      let%bind changes =
        List.map changes ~f:(fun change ->
          let%bind kind = F.required change "kind" F.string in
          match kind with
          | "created" ->
            let%bind raw = F.required change "state" Result.return in
            let%bind state = D.Document.inspect ~limits raw in
            let%bind state = Session_state_document.upgrade state ~limits in
            (match change with
             | `Object fields ->
               Ok
                 [ `Object
                     (List.map fields ~f:(fun (name, value) ->
                        ( name
                        , if String.equal name "state"
                          then D.Document.json state
                          else value )))
                 ]
             | _ -> assert false)
          | "job_changed" ->
            let%bind job = F.required change "value" Result.return in
            let%map binding = Session_state_document.legacy_model_job_target job in
            change
            :: Option.to_list
                 (Option.map binding ~f:(fun value ->
                    `Object
                      [ "kind", `String "model_job_target_restored"; "value", value ]))
          | _ -> Ok [ change ])
        |> Result.all
      in
      match payload with
      | `Object fields ->
        Ok
          (`Object
              (List.map fields ~f:(fun (name, value) ->
                 ( name
                 , if String.equal name "changes"
                   then `Array (List.concat changes)
                   else value ))))
      | _ -> F.invalid "payload" "must be an object")
  in
  let%bind ledger_step =
    D.Conversion.Step.of_function ~kind:"session.delta" ~from_version:2 ~f:(fun payload ->
      let%bind changes = F.required payload "changes" F.array in
      let%bind changes =
        List.map changes ~f:(fun change ->
          let%bind kind = F.required change "kind" F.string in
          if not (String.equal kind "created")
          then Ok change
          else (
            let%bind raw = F.required change "state" Result.return in
            let%bind state = D.Document.inspect ~limits raw in
            let%bind state = Session_state_document.upgrade state ~limits in
            match change with
            | `Object fields ->
              Ok
                (`Object
                    (List.map fields ~f:(fun (name, value) ->
                       ( name
                       , if String.equal name "state"
                         then D.Document.json state
                         else value ))))
            | _ -> F.invalid "change" "must be an object"))
        |> Result.all
      in
      match payload with
      | `Object fields ->
        Ok
          (`Object
              (List.map fields ~f:(fun (name, value) ->
                 name, if String.equal name "changes" then `Array changes else value)))
      | _ -> F.invalid "payload" "must be an object")
  in
  let%bind conversion =
    D.Conversion.create
      ~limits
      ~targets:[ "session.delta", 3 ]
      ~max_steps:2
      ~max_operations:100_000
      ~steps:[ step; ledger_step ]
  in
  D.Conversion.upgrade conversion document
;;

let codec ~limits =
  D.Domain_codec.create
    ~limits
    ~kind:"session.delta"
    ~version:3
    ~shape
    ~supported_semantics:[]
    ~decode:(fun json -> decode_payload ~limits json |> X.document_result)
    ~encode:(fun _ ->
      Error
        (D.Error.Invalid_field
           { path = []; reason = "immutable delta document is captured at construction" }))
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let decode ~limits document =
  let open Result.Let_syntax in
  let%bind document = upgrade document ~limits in
  let%map carrier = D.Domain_codec.decode (codec ~limits) document in
  { value = D.Extension_carrier.value carrier; document }
;;

let create value ~limits ~state_document =
  let open Result.Let_syntax in
  let%bind changes =
    List.map (flatten value) ~f:(atom_to_jsonaf ~limits ~state_document)
    |> Result.all
    |> X.document_result
  in
  let%bind document =
    D.Document.create
      ~limits
      ~kind:"session.delta"
      ~version:3
      ~payload:(`Object [ "changes", `Array changes ])
  in
  (* Decode authored output too: one set of validators governs both paths. *)
  decode ~limits document
;;

(* Raw immutable child records are adopted by their existing state owner. The
   typed delta remains responsible for transition semantics; this projection is
   solely the preservation path for the same admitted children. *)
let path_error path reason = D.Error.Invalid_field { path; reason }

let rec set_path json path value =
  match path, json with
  | [], _ -> Ok value
  | key :: rest, `Object fields ->
    let open Result.Let_syntax in
    let%bind current =
      List.Assoc.find fields key ~equal:String.equal
      |> Result.of_option
           ~error:(path_error path "state preservation destination is missing")
    in
    let%map changed = set_path current rest value in
    `Object
      (List.map fields ~f:(fun (name, value) ->
         name, if String.equal name key then changed else value))
  | _ -> Error (path_error path "state preservation destination is not an object")
;;

let rec get_path json = function
  | [] -> Ok json
  | key :: rest ->
    (match D.Json.field json ~name:key with
     | D.Json.Value value -> get_path value rest
     | Null ->
       if List.is_empty rest
       then Ok `Null
       else Error (path_error (key :: rest) "null preservation destination")
     | Absent -> Error (path_error (key :: rest) "missing preservation destination"))
;;

(* Component codecs are preservation projections over already validated child
   records, not another persistence family. No temporary envelope is written.
   Retargeting to the final known child avoids invalid intermediate whole states. *)
let component_codec ~limits shape =
  match
    D.Domain_codec.create
      ~limits
      ~kind:"session.state_component"
      ~version:1
      ~shape
      ~supported_semantics:[]
      ~decode:(fun json -> Ok json)
      ~encode:(fun json -> Ok json)
  with
  | Ok codec -> codec
  | Error error ->
    raise_s [%sexp "invalid component preservation codec", (error : D.Error.t)]
;;

let component_document ~limits payload =
  D.Document.create ~limits ~kind:"session.state_component" ~version:1 ~payload
;;

let project_component ~limits shape json =
  let open Result.Let_syntax in
  let codec = component_codec ~limits shape in
  let%bind document = component_document ~limits json in
  D.Domain_codec.decode codec document
;;

let retire_component ~limits shape path json =
  let%bind.Result carrier = project_component ~limits shape json in
  if D.Json.equal (D.Extension_carrier.value carrier) json
  then Ok ()
  else Error (D.Error.Extension_conflict path)
;;

let merge_component ~limits shape ~current ~incoming =
  let open Result.Let_syntax in
  let codec = component_codec ~limits shape in
  let%bind previous = project_component ~limits shape current in
  let%bind incoming = project_component ~limits shape incoming in
  let incoming =
    D.Extension_carrier.with_value incoming (D.Extension_carrier.value previous)
  in
  let%bind combined = D.Domain_codec.adopt codec ~previous ~incoming in
  let%map document = D.Domain_codec.encode codec combined in
  D.Document.payload document
;;

let patch_member ~limits ~shape payload path ~identity_field child =
  let open Result.Let_syntax in
  let%bind id =
    match D.Json.field child ~name:identity_field with
    | Value (`String id) -> Ok id
    | _ -> Error (path_error path "changed record has no storage identity")
  in
  let%bind current = get_path payload path in
  match current with
  | `Array members ->
    let found = ref false in
    let%bind members =
      List.map members ~f:(fun member ->
        match D.Json.field member ~name:identity_field with
        | Value (`String current) when String.equal current id ->
          found := true;
          merge_component ~limits shape ~current:member ~incoming:child
        | _ -> Ok member)
      |> Result.all
    in
    if !found
    then set_path payload path (`Array members)
    else (
      let%map () = retire_component ~limits shape (path @ [ id ]) child in
      payload)
  | _ -> Error (path_error path "changed-record collection is not an array")
;;

let patch_history ~limits payload child =
  let open Result.Let_syntax in
  let%bind id =
    match D.Json.field child ~name:"id" with
    | Value (`String id) -> Ok id
    | _ -> Error (path_error [ "conversation" ] "history entry lacks identity")
  in
  let rec find = function
    | [] ->
      let%map () =
        retire_component
          ~limits
          Session_record_shapes.history_entry
          [ "conversation"; id ]
          child
      in
      payload
    | field :: rest ->
      let path = [ "conversation"; field ] in
      let%bind values = get_path payload path in
      (match values with
       | `Array values
         when List.exists values ~f:(fun value ->
                match D.Json.field value ~name:"id" with
                | Value (`String other) -> String.equal other id
                | _ -> false) ->
         patch_member
           ~limits
           ~shape:Session_record_shapes.history_entry
           payload
           path
           ~identity_field:"id"
           child
       | _ -> find rest)
  in
  find [ "canonical_history"; "deferred_user_entries" ]
;;

let patch_scalar ~limits ~shape payload destination child =
  let open Result.Let_syntax in
  let%bind current = get_path payload destination in
  match current, child with
  | `Null, `Null -> Ok payload
  | `Null, _ ->
    let%map () = retire_component ~limits shape destination child in
    payload
  | _, `Null -> Ok payload
  | _, _ ->
    let%bind combined = merge_component ~limits shape ~current ~incoming:child in
    set_path payload destination combined
;;

let raw_changes t =
  let open Result.Let_syntax in
  let%bind fields = X.object_ (D.Document.payload t.document) |> X.document_result in
  X.required fields "changes" (X.list X.raw) |> X.document_result
;;

module Transaction_metadata = struct
  type t =
    { updated_at : P.Timestamp.t
    ; revision : int64
    ; transaction_sequence : int64
    ; last_event_sequence : int64 option
    }

  let create ~updated_at ~revision ~transaction_sequence ~last_event_sequence =
    let open Result.Let_syntax in
    let counters =
      [ "revision", revision; "transaction_sequence", transaction_sequence ]
      @ Option.to_list
          (Option.map last_event_sequence ~f:(fun value -> "last_event_sequence", value))
    in
    let%map () =
      List.fold_result counters ~init:() ~f:(fun () (field, value) ->
        if Int64.(value >= zero)
        then Ok ()
        else
          Error
            (D.Error.Invalid_field
               { path = [ "transaction_metadata"; field ]
               ; reason = "must be nonnegative"
               }))
    in
    { updated_at; revision; transaction_sequence; last_event_sequence }
  ;;

  let apply t ~limits document =
    let fields =
      [ [ "identity"; "updated_at" ], P.Timestamp.to_json t.updated_at
      ; [ "counters"; "revision" ], X.int64_json t.revision
      ; [ "counters"; "transaction_sequence" ], X.int64_json t.transaction_sequence
      ]
      @ Option.to_list
          (Option.map t.last_event_sequence ~f:(fun value ->
             [ "counters"; "event_sequence" ], X.int64_json value))
    in
    D.Document.replace_payload_scalars document ~limits ~updates:fields
  ;;
end

let apply t ?transaction_metadata ~limits previous =
  let open Result.Let_syntax in
  let%bind next =
    Delta.apply ~limits (Session_state_document.value previous) t.value
    |> X.document_result
  in
  let candidate = Session_state_document.with_value previous next in
  let%bind changes = raw_changes t in
  (* Created is an immutable captured state document, carrying its full template.
    Retarget it to the final typed candidate before unioning preservation paths. *)
  let%bind candidate =
    List.fold_result changes ~init:candidate ~f:(fun candidate change ->
      match D.Json.field change ~name:"kind" with
      | Value (`String "created") ->
        let%bind json = get_path change [ "state" ] in
        let%bind document = D.Document.inspect ~limits json in
        let%bind incoming = Session_state_document.decode ~limits document in
        Session_state_document.adopt
          candidate
          ~limits
          (Session_state_document.with_value incoming next)
      | _ -> Ok candidate)
  in
  let%bind document = Session_state_document.encode candidate ~limits in
  let%bind payload =
    List.fold_result changes ~init:(D.Document.payload document) ~f:(fun payload change ->
      let%bind kind =
        match D.Json.field change ~name:"kind" with
        | Value (`String value) -> Ok value
        | _ -> Error (path_error [ "changes" ] "invalid admitted change kind")
      in
      let field name = get_path change [ name ] in
      let scalar destination source shape =
        let%bind child = field source in
        patch_scalar ~limits ~shape payload destination child
      in
      let member destination key shape =
        let%bind child = field "value" in
        patch_member ~limits ~shape payload [ destination ] ~identity_field:key child
      in
      match kind with
      | "permission_changed" -> member "permissions" "id" Session_record_shapes.permission
      | "grant_changed" -> member "grants" "id" Session_record_shapes.grant
      | "model_job_target_captured"
      | "model_job_recipe_target_captured"
      | "model_job_target_restored" ->
        member "model_job_targets" "job_id" Model_job_target.shape
      | "job_changed" -> member "jobs" "id" Session_record_shapes.job
      | "schedule_changed" -> member "schedules" "id" Session_record_shapes.schedule
      | "invocation_changed" | "invocation_reconciled" ->
        member "invocations" "id" Session_record_shapes.invocation
      | "managed_submission_admitted" | "managed_submission_changed" ->
        member "managed_submissions" "history_id" Managed_submission.shape
      | "managed_stop_admitted" -> member "managed_stops" "id" Managed_stop.shape
      | "moderator_execution_changed" | "moderator_execution_reconciled" ->
        member "moderator_executions" "id" Session_record_shapes.moderator_execution
      | "subscription_changed" | "subscription_expired" | "subscription_cancelled" ->
        member "subscriptions" "id" Session_record_shapes.subscription
      | "delivery_changed" | "delivery_wake_changed" ->
        member "deliveries" "id" Session_record_shapes.delivery
      | "ingress_changed" -> member "ingress_registrations" "id" External_ingress.shape
      | "attachment_added" ->
        let%bind child = field "attachment" in
        patch_member
          ~limits
          ~shape:Session_state_document.attachment_shape
          payload
          [ "attachments" ]
          ~identity_field:"id"
          child
      | "compaction_archived" ->
        let%bind child = field "archive" in
        patch_member
          ~limits
          ~shape:Session_state_document.archive_reference_shape
          payload
          [ "conversation"; "compaction_archives" ]
          ~identity_field:"operation_id"
          child
      | "canonical_entries_appended"
      | "canonical_history_replaced"
      | "deferred_entries_enqueued" ->
        let%bind entries = field "entries" in
        (match entries with
         | `Array entries ->
           List.fold_result entries ~init:payload ~f:(patch_history ~limits)
         | _ -> Error (path_error [ "changes" ] "invalid admitted history entries"))
      | "delivery_committed" ->
        let%bind delivery = field "delivery" in
        let%bind payload =
          patch_member
            ~limits
            ~shape:Session_record_shapes.delivery
            payload
            [ "deliveries" ]
            ~identity_field:"id"
            delivery
        in
        let%bind entry = field "entry" in
        patch_history ~limits payload entry
      | "lifecycle_changed" ->
        scalar [ "lifecycle" ] "lifecycle" Session_record_shapes.lifecycle
      | "workspace_changed" ->
        scalar [ "spec"; "workspace_instance" ] "workspace" Workspace_instance.shape
      | "authoring_publication_changed" ->
        scalar
          [ "conversation"; "authoring_publication" ]
          "publication"
          Chat_response.Authoring_publication.context_shape
      | "automatic_turn_budget_enabled" ->
        scalar
          [ "automatic_turn_budget"; "policy" ]
          "policy"
          Automatic_turn_budget.policy_shape
      | "active_operation_changed" ->
        scalar [ "active_operation" ] "operation" Session_record_shapes.operation
      | "moderator_changed" ->
        scalar [ "moderator" ] "moderator" Moderator_checkpoint.shape
      | "shell_changed" -> scalar [ "shell" ] "shell" Session.Shell_state.shape
      | "failure_changed" -> scalar [ "failure" ] "failure" Session_record_shapes.error
      | _ -> Ok payload)
  in
  let json =
    match D.Document.json document with
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (name, value) ->
           name, if String.equal name "payload" then payload else value))
    | _ -> assert false
  in
  let%bind incoming_document = D.Document.inspect ~limits json in
  (* Admit the complete unstamped state first: transaction metadata must not
     repair invalid native counters or hide an oversized intermediate carrier. *)
  let%bind incoming_document =
    match transaction_metadata with
    | None -> Ok incoming_document
    | Some metadata -> Transaction_metadata.apply metadata ~limits incoming_document
  in
  (* The fold starts from the complete carried candidate. Component merges keep
     its known projection and add only compatible preservation paths, so this
     admitted document already contains the candidate and captured child fields. *)
  Session_state_document.decode ~limits incoming_document
;;
