(** Typed successful results for every Protocol 1.0 method. *)

module Server_info : sig
  type t =
    { server_id : Id.Server.t
    ; implementation : Initialize.Implementation.t
    ; protocol_version : Version.t
    ; features : string list
    ; transports : string list
    ; limits : Initialize.Limits.t
    ; unsafe_development_auth : bool
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Session_mutation : sig
  type t =
    { session : Session.t
    ; mutation : Mutation_result.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Attach : sig
  type replay =
    | Current
    | Events of Event.Durable.t list
    | Snapshot of Snapshot.t
  [@@deriving sexp]

  type t =
    { attachment : Session.Attachment.t
    ; replay : replay
    ; latest_event_sequence : int64
    ; reclaim_token : string option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Create : sig
  type t =
    { session : Session.t
    ; mutation : Mutation_result.t
    ; attachment : Attach.t option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Send_message : sig
  type disposition =
    | Started
    | Deferred
  [@@deriving compare, equal, sexp]

  type t =
    { history_id : History.Id.t
    ; disposition : disposition
    ; operation_id : Id.Operation.t option
    ; mutation : Mutation_result.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Export : sig
  type t =
    { blob : Blob.Metadata.t
    ; session_revision : int64
    ; latest_event_sequence : int64
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Delete : sig
  type t =
    { session_id : Id.Session.t
    ; deleted_at : Timestamp.t
    ; archive : Blob.Metadata.t option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t =
  | Protocol_initialize of Initialize.Response.t
  | Protocol_ping of Ping.Response.t
  | Server_info of Server_info.t
  | Server_health of Health.Response.t
  | Prompt_list of Prompt.t Page.t
  | Prompt_get of Prompt.t
  | Workspace_list of Workspace.t Page.t
  | Workspace_get of Workspace.t
  | Blob_read of Blob.Chunk.t
  | Session_create of Create.t
  | Session_list of Session.t Page.t
  | Session_get of Snapshot.t
  | Session_attach of Attach.t
  | Session_detach of Mutation_result.t
  | Session_renew_owner of Session.Owner_lease.t * Mutation_result.t
  | Session_start of Session_mutation.t
  | Session_stop of Session_mutation.t
  | Session_cancel_operation of Session_mutation.t
  | Session_send_message of Send_message.t
  | Session_compact of Session_mutation.t
  | Session_delete_history of Session_mutation.t
  | Session_export of Export.t
  | Session_reset of Session_mutation.t
  | Session_rebuild of Session_mutation.t
  | Session_upgrade_prompt of Session_mutation.t
  | Session_delete of Delete.t
  | Permission_list of Permission.t Page.t
  | Permission_respond of Permission.Respond_result.t
  | Grant_list of Grant.t Page.t
  | Grant_revoke of Grant.Revoke_result.t
  | Audit_read of Audit.t Page.t
  | Job_list of Job.t Page.t
  | Job_get of Job.t
  | Job_cancel of Job.Cancel_result.t
  | Schedule_list of Schedule.t Page.t
  | Schedule_get of Schedule.t
  | Schedule_create of Schedule.Mutation_response.t
  | Schedule_cancel of Schedule.Mutation_response.t
  | Ingress_submit of Ingress.Acknowledgement.t
[@@deriving sexp]

(** [method_name t] returns the request method associated with [t]. *)
val method_name : t -> string

(** [to_json t] encodes the method-specific result object. *)
val to_json : t -> Jsonaf.t

(** [of_json ~method_ json] decodes the successful result for [method_]. *)
val of_json : method_:string -> Jsonaf.t -> (t, Error.t) result

(** [supported_methods] contains every method with a typed success decoder. *)
val supported_methods : string list
