(** Capacity of the truthful terminal disposition of currently known Run work.
    Private JSON witnesses are not validated Runs, proofs, receipts or authority.
    Unknown future history/worker diagnostics are outside this promise; the
    bounded recovery disposition remains available if those cannot be committed. *)
type t

val capture
  :  runs:Agent_protocol.Run.t list
  -> intents:Run_intent.t list
  -> receipts:Agent_protocol.Run_receipt.t list
  -> (t, Agent_protocol.Error.t) result

val is_empty : t -> bool
val remaining_steps : t -> int

(** Fixed-count full-index projections: truthful recovery, known requested
    completion, and consumed intent shapes. No projection is decoded as a domain
    value or installed. The complete-carrier owner must merge each with its actual
    preservation codec, rejecting protected unknown incompatibility. *)
val projections : t -> index:Jsonaf.t -> Jsonaf.t list

(** Charge dimension-wise maximum positive growth of actual preserved projections.
    The owner supplies its real merge; the returned JSON never becomes state. *)
val reserved_growth
  :  t
  -> index:Jsonaf.t
  -> limits:Document_schema.Limits.t
  -> preserve:(Jsonaf.t -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> (Capacity_growth.t, Agent_protocol.Error.t) result
