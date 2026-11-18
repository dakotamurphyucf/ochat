(** Complete universal-carrier probes for bounded known recurrence control
    dispositions. These JSON witnesses grant no control/lifecycle/target authority
    and must never be decoded as state or installed. One bulk projection covers
    the owner; no per-row Cartesian copies or hypothetical execution rows. *)
type t

val capture
  :  Session_state.t
  -> shape:Document_schema.Shape.t
  -> document:Document_schema.Document.t
  -> limits:Document_schema.Limits.t
  -> (t, Document_schema.Error.t) result

val growth : t -> Capacity_growth.t
val projected_catalog : t -> Jsonaf.t option
