(** Canonical single native Goal action slot. This envelope changes no existing
    Resume request/intent encoding, digest or lifecycle. *)
type t =
  | Start of Goal_native_start_intent.t
  | Resume of Goal_native_resume_intent.t
[@@deriving equal, sexp]

val validate_update : t -> incoming:t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
val persistence_semantics : t -> string

(** Structured tagged ownership: compatible unknown intent/phase fields survive
    through the universal carrier. Each request remains wholly owned by its
    strict current codec. This shape creates no runtime custody. *)
val shape : Document_schema.Shape.t

(** Capacity DATA only: finite later-phase alternatives with frozen immutable
    request/principal/source/key. Includes conservative byte/field/node/depth
    witnesses for bounded diagnostic subtrees and future admission receipt
    envelopes; these witnesses must never be decoded as domain evidence.
    Terminal phases have no later growth. The Session owner must merge each
    projection with the complete actual carrier and take componentwise maximum
    growth across alternatives, adding across separate actions. Whole containing
    document limits always win. *)
val capacity_projections : t -> Jsonaf.t list
