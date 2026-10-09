(** Bounded lifecycle metadata for existing pending occurrences, independent of
    immutable original command receipts and managed delegation submissions. *)
module Retirement_reason : sig
  type t =
    | Source_reset
    | Source_replaced
    | Canonical_history_retired
  [@@deriving equal, sexp]
end

module Outcome : sig
  type t =
    | Adopted of Agent_protocol.History.Content_revision.t
    | Cancelled
    | Retired of Retirement_reason.t
  [@@deriving equal, sexp]
end

type t [@@deriving equal, sexp]

val create
  :  history_id:Agent_protocol.History.Id.t
  -> generation:int
  -> pending_revision:Agent_protocol.Pending_input.Revision.t
  -> outcome:Outcome.t
  -> (t, Agent_protocol.Error.t) result

val history_id : t -> Agent_protocol.History.Id.t
val generation : t -> int
val pending_revision : t -> Agent_protocol.Pending_input.Revision.t
val outcome : t -> Outcome.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result

(** Explicitly supplied record budget. Evicted outcome lookups are unavailable;
    absence never proves that an uncertain submission was not admitted. *)
module Retention : sig
  type disposition = t
  type t

  val create : max_records:int -> (t, Agent_protocol.Error.t) result

  (** Independent production policy: at most 4096 retained pending outcomes.
      Live admission and recovery share this policy; notification, delegation
      and command-receipt settings do not affect it. *)
  val default : t

  val max_records : t -> int
  val retain : t -> disposition list -> disposition list
end
