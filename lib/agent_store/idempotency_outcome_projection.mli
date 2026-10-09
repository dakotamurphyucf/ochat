open! Core

(** Captured original outcomes indexed once by stable receipt identity. Unknown
    receipt/envelope fields remain in metadata; terminal custody moves into the
    immutable artifact. *)
type t

module Prepared : sig
  type t

  val terminal : t -> Jsonaf.t
  val pending_custody : t -> Jsonaf.t option
end

(** Duplicate original identities fail closed. No repeated receipt-array scan. *)
val capture : Document_schema.Document.t option -> (t, Store_error.t) result

val raw_outcome
  :  t
  -> record_id:string
  -> authored:Jsonaf.t
  -> (Prepared.t, Store_error.t) result

(** Substitute only outcomes of stable owned record identities before the domain
    merger, so moved unknown fields cannot be dropped or duplicated. *)
val replace
  :  Document_schema.Document.t
  -> references:Idempotency_outcome.Reference.t String.Map.t
  -> limits:Document_schema.Limits.t
  -> (Document_schema.Document.t, Store_error.t) result
