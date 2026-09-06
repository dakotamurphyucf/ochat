(** Stable errors exposed by the Ochat agent protocol. *)

open Core

type code =
  | Invalid_request
  | Method_not_found
  | Unauthenticated
  | Permission_denied
  | Session_not_found
  | Prompt_not_found
  | Workspace_not_found
  | Invalid_state
  | Already_resolved
  | Resource_limit
  | Workspace_unavailable
  | Prompt_unavailable
  | Manifest_unauthorized
  | Approval_required
  | Idempotency_conflict
  | Snapshot_required
  | Operation_not_found
  | Persistence_error
  | Interrupted
  | Conflict
  | Internal_error
  | Incompatible_protocol
  | Cursor_expired
  | Blob_unavailable
  | Lease_stale
  | Configuration_invalid
  | Store_locked
  | Store_schema_too_new
  | Migration_required
  | Journal_corrupt
  | Server_shutting_down
  | Command_queue_full
[@@deriving compare, equal, sexp]

type t =
  { code : code
  ; message : string
  ; retryable : bool
  ; data : Jsonaf.t
  }
[@@deriving sexp]

(** [create code ~message ~retryable ?data ()] creates a transport-safe error. *)
val create : code -> message:string -> retryable:bool -> ?data:Jsonaf.t -> unit -> t

(** [invalid_request ?data message] creates a non-retryable invalid-request error. *)
val invalid_request : ?data:Jsonaf.t -> string -> t

(** [code_to_string code] returns the stable wire representation of [code]. *)
val code_to_string : code -> string

(** [code_of_string value] parses a stable error code. *)
val code_of_string : string -> (code, t) result

(** [to_json t] encodes [t] using the stable protocol field names. *)
val to_json : t -> Jsonaf.t

(** [of_json json] decodes a protocol error and rejects duplicate required fields. *)
val of_json : Jsonaf.t -> (t, t) result
