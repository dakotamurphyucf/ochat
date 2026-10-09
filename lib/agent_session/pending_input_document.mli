(** Immutable admitted in-memory subcarrier of the sole session-state document.
    No independent file, queue or mutation authority. The complete stored wrapper
    retains compatible unknown fields, nulls and numeric lexemes. *)
module Owner : sig
  (** Host-captured ownership. Legacy unknown is never inferred from a writer;
      orchestration provenance does not authorize user controls. *)
  type t =
    | Unknown
    | Submitting_principal of Agent_protocol.Id.Principal.t
    | Host_internal
  [@@deriving equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
  val shape : Document_schema.Shape.t

  (** Additional to current host visibility/transcript and writer permission. *)
  val authorize
    :  t
    -> principal:Agent_protocol.Id.Principal.t
    -> (unit, Agent_protocol.Error.t) result
end

type t [@@deriving sexp]

val authored
  :  ?owner:Owner.t
  -> Agent_protocol.Pending_input.t
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

(** Exact admitted wrapper equality, including retained unknown fields. *)
val equal : t -> t -> bool

val entry : t -> Agent_protocol.History.entry
val value : t -> Agent_protocol.Pending_input.t
val owner : t -> Owner.t

val with_value
  :  t
  -> Agent_protocol.Pending_input.t
  -> limits:Document_schema.Limits.t
  -> (t, Document_schema.Error.t) result

(** Exact admitted entry object and exact wrapper fields except [entry], for the
    two-destination adoption proof. These are private storage operations, never
    public pending query projections. *)
val entry_jsonaf
  :  t
  -> limits:Document_schema.Limits.t
  -> (Jsonaf.t, Document_schema.Error.t) result

val metadata_jsonaf
  :  t
  -> limits:Document_schema.Limits.t
  -> (Jsonaf.t, Document_schema.Error.t) result

val shape : Document_schema.Shape.t

(** Shared private storage shape for the validated temporal binding. *)
val binding_shape : Document_schema.Shape.t

(** Known semantic projection for the owning document codec. Retained unknowns
    remain in the original whole-state template; this does not retire custody. *)
val known_jsonaf
  :  t
  -> limits:Document_schema.Limits.t
  -> (Jsonaf.t, Document_schema.Error.t) result
