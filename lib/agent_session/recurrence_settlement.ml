open! Core

module Automatic = struct
  type t =
    | Historical_only
    | Current_policy_allows
end

module Next = struct
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
