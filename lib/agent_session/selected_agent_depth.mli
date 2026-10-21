open! Core

(** A finite policy for active synchronous selected Agent calls. It is not a
    source, native, provider, Job or Store grant. Claimed Worker ancestry and
    content/compiler recursion have separate bounds. *)
module Limits : sig
  type t

  (** [max_active_calls] counts the first claimed Saved child as one. Only
      values from 1 through 64 are admitted. *)
  val create : max_active_calls:int -> t Or_error.t

  val default : t
  val max_active_calls : t -> int
end

module Active : sig
  type t

  (** Begin the real claimed Saved child at depth one. A Child_prepared owner
      carries this immutable value through Content and Model children without
      resetting it. Neither operation mints a new runtime authority. *)
  val start : Limits.t -> t

  (** Charge one ordinary Agent Tool child before preparing its resources or
      entering any effect. Exhaustion is a typed denial; no child is installed.
      A child issued below Content or Model inherits that parent's count. *)
  val descend : t -> (t, Agent_protocol.Error.t) result

  val current : t -> int
  val limits : t -> Limits.t
end
