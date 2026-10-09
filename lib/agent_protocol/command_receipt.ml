open! Core

module Request = struct
  type t =
    { method_name : string
    ; original_params : Jsonaf.t
    }
  [@@deriving sexp]

  let validate_original_params params =
    Json_codec.validate_limits ~max_depth:256 ~max_bytes:(16 * 1024 * 1024) params
  ;;

  let to_json t = `Object [ "method", `String t.method_name; "params", t.original_params ]

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = Json_codec.fields json in
    let%bind method_name = Json_codec.required_as fields "method" Json_codec.string in
    let%bind () =
      if String.length method_name <= 256
      then Ok ()
      else Error (Protocol_error.invalid_request "receipt method name exceeds its limit")
    in
    let%bind original_params = Json_codec.required fields "params" in
    let%map () = validate_original_params original_params in
    { method_name; original_params }
  ;;
end

type committed =
  | Provider_setup of Provider_operator.Setup_result.t
  | Provider_login of Provider_operator.Flow_ref.t
  | Provider_cancel of Provider_operator.Flow_result.t
  | Provider_logout of Provider_operator.Logout_result.t
  | Provider_selection of Provider_operator.Selection_result.t
  | Provider_configuration of Provider_operator.Configuration_result.t
  | Created_session of Id.Session.t
  | Attached_session of Id.Session.t
  | Session_mutation of
      { session_id : Id.Session.t
      ; mutation : Mutation_result.t
      }
  | Configuration_updated of
      { session_id : Id.Session.t
      ; revision : int64
      }
  | Edited_history of
      { session_id : Id.Session.t
      ; history_id : History.Id.t
      ; content_revision : History.Content_revision.t
      ; archived_revision : int64
      ; continuation : History_edit.Continuation.t
      ; mutation : Mutation_result.t
      }
  | Continued_history of
      { session_id : Id.Session.t
      ; continuation : History_edit.Continuation.t
      ; mutation : Mutation_result.t
      }
  | Accepted_run of
      { session_id : Id.Session.t
      ; receipt : Run_receipt.t
      }
  | Sent_message of
      { session_id : Id.Session.t
      ; history_id : History.Id.t
      ; operation_id : Id.Operation.t option
      ; mutation : Mutation_result.t
      }
  | Project_mutation of
      { project_id : Id.Project.t
      ; revision : int64
      }
  | Deleted_project of
      { project_id : Id.Project.t
      ; revision : int64
      }
  | Collection_mutation of
      { collection_id : Id.Collection.t
      ; revision : int64
      }
  | Deleted_collection of
      { collection_id : Id.Collection.t
      ; revision : int64
      }
  | Deleted_session of Id.Session.t
  | Permission_response of Id.Permission.t * Mutation_result.t
  | Revoked_grant of Id.Grant.t * Mutation_result.t
  | Cancelled_job of Id.Job.t * Mutation_result.t
  | Schedule_mutation of Id.Schedule.t * Mutation_result.t
[@@deriving sexp]

type t =
  | Missing
  | Unavailable
  | Pending of
      { accepted_sequence : int64 option
      ; expires_at : Timestamp.t option
      }
  | Failed of Protocol_error.t
  | Committed of committed
[@@deriving sexp]

let optional_nullable fields name decode =
  match Json_codec.optional fields name with
  | None | Some `Null -> Ok None
  | Some json -> Result.map (decode json) ~f:Option.some
;;

let mutation_fields mutation = [ "mutation", Mutation_result.to_json mutation ]

let committed_to_json = function
  | Provider_setup value ->
    `Object
      [ "kind", `String "provider_setup"
      ; "value", Provider_operator.Setup_result.to_json value
      ]
  | Provider_login value ->
    `Object
      [ "kind", `String "provider_login"
      ; "value", Provider_operator.Flow_ref.to_json value
      ]
  | Provider_cancel value ->
    `Object
      [ "kind", `String "provider_cancel"
      ; "value", Provider_operator.Flow_result.to_json value
      ]
  | Provider_logout value ->
    `Object
      [ "kind", `String "provider_logout"
      ; "value", Provider_operator.Logout_result.to_json value
      ]
  | Provider_selection value ->
    `Object
      [ "kind", `String "provider_selection"
      ; "value", Provider_operator.Selection_result.to_json value
      ]
  | Provider_configuration value ->
    `Object
      [ "kind", `String "provider_configuration"
      ; "value", Provider_operator.Configuration_result.to_json value
      ]
  | Created_session id ->
    `Object [ "kind", `String "created_session"; "session_id", Id.Session.to_json id ]
  | Attached_session id ->
    `Object
      [ "kind", `String "attached_session"
      ; "session_id", Id.Session.to_json id
      ; "recovery", `String "reattach_required"
      ]
  | Project_mutation { project_id; revision } ->
    `Object
      [ "kind", `String "project_mutation"
      ; "project_id", Id.Project.to_json project_id
      ; "revision", `Number (Core.Int64.to_string revision)
      ]
  | Deleted_project { project_id; revision } ->
    `Object
      [ "kind", `String "deleted_project"
      ; "project_id", Id.Project.to_json project_id
      ; "revision", `Number (Core.Int64.to_string revision)
      ]
  | Collection_mutation { collection_id; revision } ->
    `Object
      [ "kind", `String "collection_mutation"
      ; "collection_id", Id.Collection.to_json collection_id
      ; "revision", `Number (Core.Int64.to_string revision)
      ]
  | Deleted_collection { collection_id; revision } ->
    `Object
      [ "kind", `String "deleted_collection"
      ; "collection_id", Id.Collection.to_json collection_id
      ; "revision", `Number (Core.Int64.to_string revision)
      ]
  | Deleted_session id ->
    `Object [ "kind", `String "deleted_session"; "session_id", Id.Session.to_json id ]
  | Session_mutation { session_id; mutation } ->
    `Object
      ([ "kind", `String "session_mutation"; "session_id", Id.Session.to_json session_id ]
       @ mutation_fields mutation)
  | Configuration_updated { session_id; revision } ->
    `Object
      [ "kind", `String "configuration_updated"
      ; "session_id", Id.Session.to_json session_id
      ; "revision", `Number (Int64.to_string revision)
      ]
  | Edited_history
      { session_id
      ; history_id
      ; content_revision
      ; archived_revision
      ; continuation
      ; mutation
      } ->
    `Object
      ([ "kind", `String "edited_history"
       ; "session_id", Id.Session.to_json session_id
       ; "history_id", History.Id.to_json history_id
       ; "content_revision", History.Content_revision.to_json content_revision
       ; "archived_revision", `Number (Int64.to_string archived_revision)
       ; "continuation", History_edit.Continuation.to_json continuation
       ]
       @ mutation_fields mutation)
  | Continued_history { session_id; continuation; mutation } ->
    `Object
      ([ "kind", `String "continued_history"
       ; "session_id", Id.Session.to_json session_id
       ; "continuation", History_edit.Continuation.to_json continuation
       ]
       @ mutation_fields mutation)
  | Accepted_run { session_id; receipt } ->
    `Object
      [ "kind", `String "accepted_run"
      ; "session_id", Id.Session.to_json session_id
      ; "receipt", Run_receipt.to_json receipt
      ]
  | Sent_message { session_id; history_id; operation_id; mutation } ->
    `Object
      ([ "kind", `String "sent_message"
       ; "session_id", Id.Session.to_json session_id
       ; "history_id", History.Id.to_json history_id
       ; ( "operation_id"
         , Option.value_map operation_id ~default:`Null ~f:Id.Operation.to_json )
       ]
       @ mutation_fields mutation)
  | Permission_response (id, mutation) ->
    `Object
      ([ "kind", `String "permission_response"
       ; "permission_id", Id.Permission.to_json id
       ]
       @ mutation_fields mutation)
  | Revoked_grant (id, mutation) ->
    `Object
      ([ "kind", `String "revoked_grant"; "grant_id", Id.Grant.to_json id ]
       @ mutation_fields mutation)
  | Cancelled_job (id, mutation) ->
    `Object
      ([ "kind", `String "cancelled_job"; "job_id", Id.Job.to_json id ]
       @ mutation_fields mutation)
  | Schedule_mutation (id, mutation) ->
    `Object
      ([ "kind", `String "schedule_mutation"; "schedule_id", Id.Schedule.to_json id ]
       @ mutation_fields mutation)
;;

let committed_of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  let%bind kind = Json_codec.required_as fields "kind" Json_codec.string in
  let session () = Json_codec.required_as fields "session_id" Id.Session.of_json in
  let mutation () = Json_codec.required_as fields "mutation" Mutation_result.of_json in
  match kind with
  | "provider_setup" ->
    Json_codec.required_as fields "value" Provider_operator.Setup_result.of_json
    |> Result.map ~f:(fun value -> Provider_setup value)
  | "provider_login" ->
    Json_codec.required_as fields "value" Provider_operator.Flow_ref.of_json
    |> Result.map ~f:(fun value -> Provider_login value)
  | "provider_cancel" ->
    Json_codec.required_as fields "value" Provider_operator.Flow_result.of_json
    |> Result.map ~f:(fun value -> Provider_cancel value)
  | "provider_logout" ->
    Json_codec.required_as fields "value" Provider_operator.Logout_result.of_json
    |> Result.map ~f:(fun value -> Provider_logout value)
  | "provider_selection" ->
    Json_codec.required_as fields "value" Provider_operator.Selection_result.of_json
    |> Result.map ~f:(fun value -> Provider_selection value)
  | "provider_configuration" ->
    Json_codec.required_as fields "value" Provider_operator.Configuration_result.of_json
    |> Result.map ~f:(fun value -> Provider_configuration value)
  | "created_session" -> session () |> Result.map ~f:(fun id -> Created_session id)
  | "attached_session" ->
    let%bind recovery = Json_codec.required_as fields "recovery" Json_codec.string in
    if not (String.equal recovery "reattach_required")
    then Error (Protocol_error.invalid_request "invalid attachment receipt recovery")
    else session () |> Result.map ~f:(fun id -> Attached_session id)
  | "project_mutation" ->
    let%bind project_id = Json_codec.required_as fields "project_id" Id.Project.of_json in
    let%map revision =
      Json_codec.required_as
        fields
        "revision"
        (Json_codec.bounded_int64 ~min:0L ~max:Core.Int64.max_value)
    in
    Project_mutation { project_id; revision }
  | "deleted_project" ->
    let%bind project_id = Json_codec.required_as fields "project_id" Id.Project.of_json in
    let%map revision =
      Json_codec.required_as
        fields
        "revision"
        (Json_codec.bounded_int64 ~min:0L ~max:Core.Int64.max_value)
    in
    Deleted_project { project_id; revision }
  | "collection_mutation" ->
    let%bind collection_id =
      Json_codec.required_as fields "collection_id" Id.Collection.of_json
    in
    let%map revision =
      Json_codec.required_as
        fields
        "revision"
        (Json_codec.bounded_int64 ~min:0L ~max:Core.Int64.max_value)
    in
    Collection_mutation { collection_id; revision }
  | "deleted_collection" ->
    let%bind collection_id =
      Json_codec.required_as fields "collection_id" Id.Collection.of_json
    in
    let%map revision =
      Json_codec.required_as
        fields
        "revision"
        (Json_codec.bounded_int64 ~min:0L ~max:Core.Int64.max_value)
    in
    Deleted_collection { collection_id; revision }
  | "deleted_session" -> session () |> Result.map ~f:(fun id -> Deleted_session id)
  | "session_mutation" ->
    let%bind session_id = session () in
    let%map mutation = mutation () in
    Session_mutation { session_id; mutation }
  | "configuration_updated" ->
    let%bind session_id = session () in
    let%map revision =
      Json_codec.required_as
        fields
        "revision"
        (Json_codec.bounded_int64 ~min:0L ~max:Int64.max_value)
    in
    Configuration_updated { session_id; revision }
  | "edited_history" ->
    let%bind session_id = session () in
    let%bind history_id = Json_codec.required_as fields "history_id" History.Id.of_json in
    let%bind content_revision =
      Json_codec.required_as fields "content_revision" History.Content_revision.of_json
    in
    let%bind archived_revision =
      Json_codec.required_as
        fields
        "archived_revision"
        (Json_codec.bounded_int64 ~min:0L ~max:Int64.max_value)
    in
    let%bind continuation =
      Json_codec.required_as fields "continuation" History_edit.Continuation.of_json
    in
    let%map mutation = mutation () in
    Edited_history
      { session_id
      ; history_id
      ; content_revision
      ; archived_revision
      ; continuation
      ; mutation
      }
  | "continued_history" ->
    let%bind session_id = session () in
    let%bind continuation =
      Json_codec.required_as fields "continuation" History_edit.Continuation.of_json
    in
    let%map mutation = mutation () in
    Continued_history { session_id; continuation; mutation }
  | "accepted_run" ->
    let%bind session_id = session () in
    let%map receipt = Json_codec.required_as fields "receipt" Run_receipt.of_json in
    Accepted_run { session_id; receipt }
  | "sent_message" ->
    let%bind session_id = session () in
    let%bind history_id = Json_codec.required_as fields "history_id" History.Id.of_json in
    let%bind operation_id =
      optional_nullable fields "operation_id" Id.Operation.of_json
    in
    let%map mutation = mutation () in
    Sent_message { session_id; history_id; operation_id; mutation }
  | "permission_response" ->
    let%bind id = Json_codec.required_as fields "permission_id" Id.Permission.of_json in
    let%map mutation = mutation () in
    Permission_response (id, mutation)
  | "revoked_grant" ->
    let%bind id = Json_codec.required_as fields "grant_id" Id.Grant.of_json in
    let%map mutation = mutation () in
    Revoked_grant (id, mutation)
  | "cancelled_job" ->
    let%bind id = Json_codec.required_as fields "job_id" Id.Job.of_json in
    let%map mutation = mutation () in
    Cancelled_job (id, mutation)
  | "schedule_mutation" ->
    let%bind id = Json_codec.required_as fields "schedule_id" Id.Schedule.of_json in
    let%map mutation = mutation () in
    Schedule_mutation (id, mutation)
  | _ -> Error (Protocol_error.invalid_request "unknown committed receipt kind")
;;

let to_json = function
  | Missing -> `Object [ "status", `String "missing" ]
  | Unavailable -> `Object [ "status", `String "unavailable" ]
  | Pending { accepted_sequence; expires_at } ->
    `Object
      [ "status", `String "pending"
      ; ( "accepted_sequence"
        , Option.value_map accepted_sequence ~default:`Null ~f:(fun value ->
            `Number (Int64.to_string value)) )
      ; "expires_at", Option.value_map expires_at ~default:`Null ~f:Timestamp.to_json
      ]
  | Failed error ->
    `Object [ "status", `String "failed"; "error", Protocol_error.to_json error ]
  | Committed summary ->
    `Object [ "status", `String "committed"; "summary", committed_to_json summary ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = Json_codec.validate_limits ~max_depth:64 ~max_bytes:65536 json in
  let%bind fields = Json_codec.fields json in
  let%bind status = Json_codec.required_as fields "status" Json_codec.string in
  match status with
  | "missing" -> Ok Missing
  | "unavailable" -> Ok Unavailable
  | "pending" ->
    let%bind accepted_sequence =
      optional_nullable
        fields
        "accepted_sequence"
        (Json_codec.bounded_int64 ~min:0L ~max:Int64.max_value)
    in
    let%map expires_at = optional_nullable fields "expires_at" Timestamp.of_json in
    Pending { accepted_sequence; expires_at }
  | "failed" ->
    Json_codec.required_as fields "error" Protocol_error.of_json
    |> Result.map ~f:(fun error -> Failed error)
  | "committed" ->
    Json_codec.required_as fields "summary" committed_of_json
    |> Result.map ~f:(fun summary -> Committed summary)
  | _ -> Error (Protocol_error.invalid_request "unknown command receipt status")
;;
