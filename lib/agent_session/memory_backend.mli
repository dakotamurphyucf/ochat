open! Core

(** Process-local state backend with the same commit-before-publish boundary as
    durable storage. It is used by embedded process-bound sessions. *)

type t

val create : event_capacity:int -> initial_state:Session_state.t -> t
val persistence : t -> Session_actor.persistence
val state : t -> Session_state.t
val archived_state : t -> revision:int64 -> Session_state.t option

val events_after
  :  t
  -> int64
  -> (Agent_protocol.Event.Durable.t list, Agent_protocol.Error.t) result
