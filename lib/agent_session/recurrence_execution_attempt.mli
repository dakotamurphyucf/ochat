open! Core

(** Immutable changing fields of one admission attempt. Input and selected
    nominal stay in the occurrence's shared intent. Restored data grants no
    permission to retry, initialize a runtime or admit a Run. *)
type t

(** At most eight attempts per occurrence, numbered 0 through 7. This finite
    profile bounds retained request identities; available storage/grace may hold
    an occurrence earlier. Busy preflight before freezing an attempt consumes no
    number. Only a genuine staged owner's no-Run/no-effects resolution can admit
    another attempt; the execution owner enforces that separate transition. *)
val maximum_count : int

val create
  :  intent:Recurrence_execution_intent.t
  -> number:int
  -> principal_id:Agent_protocol.Id.Principal.t
  -> expected_revision:int64
  -> key:Agent_protocol.Idempotency_key.t
  -> (t, Agent_protocol.Error.t) result

val number : t -> int
val principal_id : t -> Agent_protocol.Id.Principal.t
val expected_revision : t -> int64
val key : t -> Agent_protocol.Idempotency_key.t
val intent_sha256 : t -> string
val request_sha256 : t -> string
val digest : t -> string

(** Checks the exact shared intent before reconstructing the existing canonical
    request. Owned constructor/controller commits may advance an actual target
    preparation without rewriting this original CAS or digest. *)
val request
  :  t
  -> intent:Recurrence_execution_intent.t
  -> (Run_admission_request.Headless.t, Agent_protocol.Error.t) result

val validate
  :  t
  -> intent:Recurrence_execution_intent.t
  -> (unit, Agent_protocol.Error.t) result

val equal : t -> t -> bool
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val limits : Document_schema.Limits.t
