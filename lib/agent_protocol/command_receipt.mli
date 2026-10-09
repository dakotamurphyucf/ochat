(** Read-only reconciliation of an original bounded request. The authenticated
    principal is implicit. Original params prove mode/authority and digest;
    lookup never admits, retries or replays the command. *)
module Request : sig
  type t =
    { method_name : string
    ; original_params : Jsonaf.t
    }
  [@@deriving sexp]

  (** Shared admission/receipt bounds apply to original parameters; the small
      receipt envelope does not consume their byte or nesting allowance. *)
  val validate_original_params : Jsonaf.t -> (unit, Error.t) result

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

(** Narrow result references, never a replay of a full original result.
    Create/attach recovery must use a fresh authorized attachment. *)
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
  | Failed of Error.t
  | Committed of committed
[@@deriving sexp]

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
