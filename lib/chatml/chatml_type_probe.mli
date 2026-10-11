open Core
open Chatml_inferred_type

module Error : sig
  type t =
    | Already_committed
    | Original_changed
    | Escaping_parameter of string
  [@@deriving sexp]
end

(** A private copy of an inference graph for module-interface matching.
    All roots are copied together, preserving shared variable identities and
    cycles. Immutable generic parameters retain their identities. The caller
    owns the original graph exclusively until committing or discarding the
    probe, and must unify only copied roots with fresh specification types.

    Physical-identity association lists avoid hashing mutable type contents.
    Variable lookup is linear in the copied variable count; traversals poll the
    compiler checkpoint, including inside identity searches. This machinery is
    isolated from ordinary expression inference. *)
type t

val create : typ list -> checkpoint:(unit -> unit) -> t

(** Mutable inference types owned by the probe, in the original root order. *)
val types : t -> typ list

(** Reject if any originally free variable now reaches one of [rigid_names],
    or if the original graph changed since copying. Otherwise commit the copied
    constraints to the original cells while preserving their identities.
    Fresh variables introduced during probing are copied on commit; mutating
    the probe afterward cannot mutate committed types through those cells.

    Failures and checkpoint exceptions leave originals untouched. Validation
    and allocation finish before a short non-cancellable mutation phase,
    linear in the original variable count. A probe commits at most once.
    This checks escape/transaction invariants only; the interface matcher is
    responsible for type compatibility and the complete set of rigid names. *)
val commit
  :  t
  -> rigid_names:String.Set.t
  -> checkpoint:(unit -> unit)
  -> (unit, Error.t) result
