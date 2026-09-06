open! Core

(** Fiber-friendly UI approval broker for shell command requests. *)

type ui_request =
  { id : string
  ; request : Shell_access.Approval.request
  ; runtime_id : string
  ; manifest_sha256 : string
  ; scopes : Chatmd_shell_spec.Shell_spec.approval_scope list
  }

type ui_response =
  | Approve_once
  | Approve_exact_session
  | Approve_prefix_session of string list
  | Approve_durable_exact
  | Deny of string

type error =
  | Closed
  | Unknown_request of string
  | Duplicate_request of string
[@@deriving sexp, compare, equal]

type t

type provider =
  | None_available
  | Auto_deny
  | Callback of t
  | Assume_approved

(** [create ?on_pending ()] creates a broker. [on_pending] runs after a new
    request becomes visible and must not block waiting for a response. *)
val create : ?on_pending:(ui_request -> unit) -> unit -> t

(** [request t request] waits cooperatively until the request is answered,
    cancelled, or the broker closes. *)
val request : t -> ui_request -> Shell_access.Approval.response

(** [pending t] returns the oldest unanswered request. *)
val pending : t -> ui_request option

val pending_all : t -> ui_request list
val pending_count : t -> int

(** [respond t ~id response] resolves one pending request. *)
val respond : t -> id:string -> ui_response -> (unit, error) result

(** [cancel t ~id] denies and removes a pending request. *)
val cancel : t -> id:string -> unit

(** [close t] denies every pending request and rejects future requests. *)
val close : t -> unit

(** [reviewer provider ~runtime_id ~manifest_sha256] creates the reviewer used
    by [Shell_access.Executor]. [None_available] returns [None], preserving the
    executor's typed permission-required result. *)
val reviewer
  :  provider
  -> runtime_id:string
  -> manifest_sha256:string
  -> scopes:Chatmd_shell_spec.Shell_spec.approval_scope list
  -> Shell_access.Approval.reviewer option
