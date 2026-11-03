open! Core

module Reason = struct
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
