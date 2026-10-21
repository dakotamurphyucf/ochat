open! Core
module Row = Developer_command_row

(** Private immutable command collection in the one developer-run document.
    It owns no persistence, authority or independent work allowance. *)
type t

val empty : t
val rows : t -> Row.t list
val find : t -> Agent_protocol.Idempotency_key.t -> Row.t option
val count : t -> int
val encoded_bytes : t -> int

(** Enforce unique keys/IDs and aggregate complete-row/terminal bounds.
    [check_owner] is the enclosing document's structural stable-owner and
    original epoch/capture check, never a live authorization callback. [poll]
    charges the caller's single traversal budget. *)
val of_rows
  :  Row.t list
  -> commands:Developer_command_limits.t
  -> poll:(unit -> unit)
  -> check_owner:(Row.t -> (unit, Store_error.t) result)
  -> (t, Store_error.t) result

val append
  :  t
  -> Row.t
  -> commands:Developer_command_limits.t
  -> poll:(unit -> unit)
  -> check_owner:(Row.t -> (unit, Store_error.t) result)
  -> (t, Store_error.t) result

(** Match the exact immutable previous row, then apply the shared phase
    validator. A changed or missing previous value conflicts; no row is
    synthesized from only its key. *)
val change
  :  t
  -> expected:Row.t
  -> change:Row.Change.t
  -> limits:Document_schema.Limits.t
  -> commands:Developer_command_limits.t
  -> poll:(unit -> unit)
  -> check_owner:(Row.t -> (unit, Store_error.t) result)
  -> (t, Row.Error.t) result

(** Produce the exact comparison collection with every nonterminal command
    Interrupted, preserving each execution_started flag. Return the number of
    row transitions so the document reserves their revision increments jointly
    with inference interruption. This is serialization headroom computation,
    never a persisted terminal decision or a runtime recovery action. *)
val interrupted
  :  t
  -> limits:Document_schema.Limits.t
  -> commands:Developer_command_limits.t
  -> poll:(unit -> unit)
  -> (t * int, Store_error.t) result
