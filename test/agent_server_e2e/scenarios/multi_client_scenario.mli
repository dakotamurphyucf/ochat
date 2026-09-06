open Core

(** Check mixed-transport attachment authority and concurrency. Writer checks
    compare every accepted user ID and complete payload in acceptance order,
    across canonical history plus its durable deferred queue, before and after
    daemon restart. Equal-authority observers compare complete durable events. *)
val run : Eio_unix.Stdenv.base -> case:string option -> unit
