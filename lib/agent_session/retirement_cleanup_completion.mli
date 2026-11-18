(** Immutable actual cleanup invocation data. The creating-domain producer
    records this pair once, at its own return/catch boundary before publishing
    completion or releasing its Running ownership. It grants no authority to
    execute cleanup or renew admission. *)
module Boundary : sig
  type t =
    | Before_release
    | Released
end

type t =
  { reply : unit Actor_reply.t
  ; boundary : Boundary.t
  }
