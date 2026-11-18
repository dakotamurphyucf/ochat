(** Private complete-carrier capacity probes. The caller supplies the exact
    admitted document for [state] and the real owning state shape/profile. Returned
    documents are JSON shape witnesses only; never decode or install them. *)
type t

val capture
  :  Session_state.t
  -> shape:Document_schema.Shape.t
  -> document:Document_schema.Document.t
  -> limits:Document_schema.Limits.t
  -> (t, Document_schema.Error.t) result

val growth : t -> Capacity_growth.t
val additional_growth : t -> Capacity_growth.t
val projections : t -> Document_schema.Document.t list
