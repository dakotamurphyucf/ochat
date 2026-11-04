open! Core

(** Immutable reciprocal association between an occurrence attempt and an actual
    Run admission. The intent and attempt digests bind their complete original
    custody without copying input, source audit metadata or captured artifacts.
    Constructed or decoded data is never execution authority or an observation
    issued by the actual target actor. *)
type t

val create
  :  intent:Recurrence_execution_intent.t
  -> attempt:Recurrence_execution_attempt.t
  -> run:Agent_protocol.Run.t
  -> admission:Agent_protocol.Run_receipt.t
  -> (t, Agent_protocol.Error.t) result

val occurrence : t -> Recurrence_identity.Occurrence.t
val intent_sha256 : t -> string
val attempt_sha256 : t -> string
val session : t -> Agent_protocol.Session_ref.t
val mode : t -> Agent_protocol.Run.Mode.t
val run_id : t -> Agent_protocol.Id.Run.t
val admission : t -> Agent_protocol.Run_receipt.t
val digest : t -> string

(** Validate association with the exact retained normalized content and original
    caller/CAS/key. The actual owner separately authenticates current authority. *)
val validate
  :  t
  -> intent:Recurrence_execution_intent.t
  -> attempt:Recurrence_execution_attempt.t
  -> (unit, Agent_protocol.Error.t) result

(** Validate supplied actual retained Run/receipt evidence, including exact
    admission equality and immutable Run identity/source/principal/mode. The
    Admission Run revision is zero, as in the existing admission producer. A later
    Run revision is allowed; neither this check nor data grants execution. *)
val validate_run
  :  t
  -> run:Agent_protocol.Run.t
  -> admission:Agent_protocol.Run_receipt.t
  -> (unit, Agent_protocol.Error.t) result

val equal : t -> t -> bool
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result

(** Complete owned carrier including compatible unknowns: 8 KiB, depth 24,
    128 fields and 256 nodes. Enclosing shared storage profiles apply separately.
    Raw JSON is bounded before decoding or hashing. *)
val limits : Document_schema.Limits.t
