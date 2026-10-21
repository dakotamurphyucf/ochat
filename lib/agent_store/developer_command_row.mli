open! Core
module P = Agent_protocol
module Receipt = Developer_command_receipt

(** Pure comparison row in the existing developer-run document. It grants no
    live execution or result disclosure. The enclosing document checks exact
    principal/run/epoch/capture membership and key uniqueness across epochs. *)
type t

module Phase : sig
  type t =
    | Reserved
    | Waiting_permission
    | Execution_started
    | Terminal of
        { execution_started : bool
        ; receipt : Receipt.t
        }
end

module Change : sig
  type t =
    | Wait_permission
    | Start_execution
    | Finish of Receipt.t
end

module Error : sig
  type t =
    | Conflict
    | Store of Store_error.t
end

(** Initial Reserved row only. The physical Store proves that this key was
    absent under its writer and allocates [id] once. The request fingerprint
    binds the original action/payload: kind, route or source selector, requested
    environment and editor revision, entry/input/fixtures and chosen
    execution/output policy. It excludes recaptured source bytes and mutable
    host policy. [capture_fingerprint] separately pins the resolved capture;
    the host owns both canonical projections. *)
val create
  :  id:Receipt.Id.t
  -> key:P.Idempotency_key.t
  -> owner:P.Execution_owner.developer
  -> capture_fingerprint:Chatml_source.Digest.t
  -> request_fingerprint:Chatml_source.Digest.t
  -> kind:Receipt.Kind.t
  -> limits:Document_schema.Limits.t
  -> poll:(unit -> unit)
  -> (t, Store_error.t) result

val id : t -> Receipt.Id.t
val key : t -> P.Idempotency_key.t
val owner : t -> P.Execution_owner.developer
val capture_fingerprint : t -> Chatml_source.Digest.t
val request_fingerprint : t -> Chatml_source.Digest.t
val kind : t -> Receipt.Kind.t
val phase : t -> Phase.t
val encoded_bytes : t -> int
val same_identity : t -> t -> bool

(** Finite phase invariants shared by mutation and decoding. Live_run and
    Repl_task_run
    waits or crosses Execution_started. A Live success requires that barrier;
    failure/limited/cancelled/interrupted may finish before it. Check and
    Fixture terminals never claim live execution. The receipt kind must match
    the row. A matching terminal is idempotent; a changed terminal or attempt
    to reopen execution conflicts. Execution_started means potential effects
    were admitted, not that a particular external effect actually happened.
    This pure transformation does not grant the live host authority required
    by the Store's reserve/start before-rename checks. *)
val apply
  :  t
  -> Change.t
  -> limits:Document_schema.Limits.t
  -> poll:(unit -> unit)
  -> (t, Error.t) result

val to_json : t -> Jsonaf.t

val of_json
  :  Jsonaf.t
  -> limits:Document_schema.Limits.t
  -> poll:(unit -> unit)
  -> (t, Store_error.t) result
