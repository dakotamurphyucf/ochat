(** Private storage subcarrier of the sole session-state authority. Public queries
    project only [value]. Explicit custody remains immutable across status changes. *)
type t [@@deriving sexp]

val equal : t -> t -> bool
val value : t -> Pending_disposition.t

val authored
  :  Pending_disposition.t
  -> limits:Document_schema.Limits.t
  -> (t, Document_schema.Error.t) result

val adopted
  :  Pending_input_document.t
  -> disposition:Pending_disposition.t
  -> limits:Document_schema.Limits.t
  -> (t, Document_schema.Error.t) result

val with_value
  :  t
  -> Pending_disposition.t
  -> limits:Document_schema.Limits.t
  -> (t, Document_schema.Error.t) result

val of_jsonaf
  :  Jsonaf.t
  -> limits:Document_schema.Limits.t
  -> (t, Document_schema.Error.t) result

val to_jsonaf
  :  t
  -> limits:Document_schema.Limits.t
  -> (Jsonaf.t, Document_schema.Error.t) result

val shape : Document_schema.Shape.t

(** Capture immutable submitting ownership and wrapper custody for a removed
    pending occurrence. Its raw entry retirement additionally requires the exact
    prestate archive, as validated by the whole-state transition. *)
val retired
  :  Pending_input_document.t
  -> disposition:Pending_disposition.t
  -> limits:Document_schema.Limits.t
  -> (t, Document_schema.Error.t) result

val owner : t -> Pending_input_document.Owner.t

(** A proved canonical retirement changes only a retained adopted outcome;
    immutable ownership/custody and all compatible extensions remain intact. *)
val retire_canonical
  :  t
  -> limits:Document_schema.Limits.t
  -> (t, Document_schema.Error.t) result

(** Known semantic projection for the owning document codec. Retained unknowns
    remain in the original whole-state template; this does not retire custody. *)
val known_jsonaf
  :  t
  -> limits:Document_schema.Limits.t
  -> (Jsonaf.t, Document_schema.Error.t) result
