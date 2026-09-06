(** Fiber-safe conflict-domain leases for shared and exclusive workspaces. *)

type mode =
  | Shared
  | Exclusive
[@@deriving compare, equal, sexp]

type lease
type t

val create : unit -> t

val acquire
  :  t
  -> conflict_domain:string
  -> session_id:Agent_protocol.Id.Session.t
  -> mode:mode
  -> (lease, Agent_protocol.Error.t) result

val release : t -> lease -> unit
val active : t -> conflict_domain:string -> int
