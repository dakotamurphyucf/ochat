open! Core

(** Private request transport. A protocol failure is a returned value; an
    unexpected owner exception retains its original backtrace and is re-raised
    by the waiting caller. Neither represents permission to replay a request. *)
type 'a t

val resolve : 'a t Eio.Promise.u -> ('a, Agent_protocol.Error.t) result -> unit
val resolve_exception : 'a t Eio.Promise.u -> exn -> Stdlib.Printexc.raw_backtrace -> unit
val await : 'a t Eio.Promise.t -> ('a, Agent_protocol.Error.t) result
