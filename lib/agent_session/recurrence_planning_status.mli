(** Process-local planning availability. These diagnostics grant no execution
    authority and are never persisted as recurrence control or outcome evidence. *)
module Reason : sig
  type t =
    | Not_configured
    | Not_granted
    | Expired
    | Closing
    | Lifecycle_not_automatic
    | Controller_not_running
    | Source_unavailable
  [@@deriving equal, sexp_of]
end

type t =
  | Available
  | Unavailable of Reason.t
[@@deriving equal, sexp_of]
