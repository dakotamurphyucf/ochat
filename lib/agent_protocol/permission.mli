(** Durable tool-authorization requests and compare-and-set responses. *)

type state =
  | Pending
  | Approved
  | Denied
  | Expired
  | Cancelled
[@@deriving compare, equal, sexp]

type choice =
  | Approve_once
  | Approve_session
  | Approve_prefix
  | Durable_exact
  | Deny
[@@deriving compare, equal, sexp]

(** An operation-owned legacy request or a request from an actual persisted
    tool invocation. Invocation ownership does not require a model operation. *)
type owner =
  | Operation of Id.Operation.t
  | Invocation of Id.Invocation.t
[@@deriving equal, sexp]

type t =
  { id : Id.Permission.t
  ; session_id : Id.Session.t
  ; generation : int
  ; owner : owner
  ; call_id : string
  ; tool_name : string
  ; runtime_identity : string option
  ; invocation_display : string
  ; rationale : string option
  ; effects : string list
  ; choices : choice list
  ; created_at : Timestamp.t
  ; expires_at : Timestamp.t option
  ; state : state
  ; resolution : resolution option
  }
[@@deriving sexp]

and resolution =
  { choice : choice
  ; principal_id : Id.Principal.t option
  ; resolved_at : Timestamp.t
  ; reason : string option
  }
[@@deriving sexp]

module Resolution : sig
  type t = resolution [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

val to_json : t -> Jsonaf.t

(** Accept legacy [operation_id] or [invocation_id], exactly one. The S-expression
    reader also accepts the old [operation_id] record field. *)
val of_json : Jsonaf.t -> (t, Error.t) result

module List_request : sig
  type t =
    { session_id : Id.Session.t
    ; page : Page.Request.t
    ; state : state option
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Respond_request : sig
  type t =
    { session_id : Id.Session.t
    ; attachment_id : Id.Attachment.t
    ; permission_id : Id.Permission.t
    ; permission_generation : int
    ; choice : choice
    ; reason : string option
    ; idempotency_key : Idempotency_key.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Respond_result : sig
  type nonrec t =
    { permission : t
    ; mutation : Mutation_result.t
    }
  [@@deriving sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
