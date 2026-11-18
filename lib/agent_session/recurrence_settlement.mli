(** Immutable result of a pure occurrence-evidence transition. None of these
    values authenticates a target observation, authorizes a retry or grants
    current planning/execution permission. *)
module Automatic : sig
  (** Disposition data supplied by the actual importing owner after fresh policy,
      source and lifecycle checks. Stored creator/control audit is not proof. *)
  type t =
    | Historical_only
    | Current_policy_allows
end

module Next : sig
  type hold =
    | Interrupted
    | Successor_exhausted
  [@@deriving equal, sexp_of]

  type t =
    | Evidence_only
    | Successor of Recurrence_delay_basis.t
    | Review_hold of hold
end

type t =
  | Imported of
      { execution : Recurrence_execution.t
      ; next : Next.t
      }
  | Retained_exact of Recurrence_execution.t
  | Detail_not_retained of Recurrence_identity.Occurrence.t

(** Detail_not_retained makes no prior completion, matching receipt, settlement
    count or execution claim. It changes no state and never rearms. A consumed
    high-water and lifetime aggregate cannot classify a missing old coordinate.
    Retained_exact preserves the complete supplied original evidence; the actual
    importer must authenticate saved extended references against the real target,
    rather than reconstructing or normalizing their compatible metadata. *)
