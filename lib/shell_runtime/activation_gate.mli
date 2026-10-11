open Core

(** Native registry execution lifetime. One owner advances Pending to Ready;
    failure or scope closure is terminal. Checks are nonyielding and may be
    retained by actual executor configurations across caller scopes. *)
type t

val create : unit -> t
val check : t -> (unit, string) Result.t
val ready : t -> (unit, string) Result.t
val fail : t -> unit
val close : t -> unit
