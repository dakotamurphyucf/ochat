open! Core

(** Compact historical terminal reference. The authoritative complete terminal
    payload remains in the actual target Run owner, including any result history.
    Restored classification is unresolved data, never an authenticated actor
    observation, achievement proof, retry permission or rearm authority. *)
type t

module Outcome : sig
  type t =
    | Completed
    | Failed of Agent_protocol.Error.code option
    | Cancelled
    | Limited
    | Interrupted
  [@@deriving equal, sexp_of]
end

(** Capture from the supplied actual terminal Run and receipt. Validates exact
    origin/source/principal/mode/revisions, cached outcome and canonical terminal
    digest using the actual complete payload. The actor must authenticate that
    these are its actual retained values before issuing an observation. *)
val create
  :  origin:Recurrence_run_origin.t
  -> run:Agent_protocol.Run.t
  -> receipt:Agent_protocol.Run_receipt.t
  -> (t, Agent_protocol.Error.t) result

val origin_sha256 : t -> string
val run_id : t -> Agent_protocol.Id.Run.t
val receipt : t -> Agent_protocol.Run_receipt.t
val outcome : t -> Outcome.t

(** Structural association with the retained origin only. This does not check
    the actual terminal payload, classify an authenticated outcome or grant an
    observation. [validate] is still required at every actual live import. *)
val validate_origin
  :  t
  -> origin:Recurrence_run_origin.t
  -> (unit, Agent_protocol.Error.t) result

(** Every live import/reconciliation resolves the actual target payload and uses
    this validation; a decoded Completed tag alone cannot satisfy it. Terminal
    Run revision strictly advances Admission; session revision cannot regress.
    No additional wall-clock ordering is inferred from a terminal reference. *)
val validate
  :  t
  -> origin:Recurrence_run_origin.t
  -> run:Agent_protocol.Run.t
  -> receipt:Agent_protocol.Run_receipt.t
  -> (unit, Agent_protocol.Error.t) result

val equal : t -> t -> bool
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result

(** Complete owned carrier including compatible unknowns: 8 KiB, depth 24,
    128 fields and 256 nodes. It contains one origin digest, not an origin copy.
    Raw JSON is bounded before decoding or hashing. *)
val limits : Document_schema.Limits.t
