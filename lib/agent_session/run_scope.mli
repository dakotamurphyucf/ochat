(** One process-local callback scope admitted by the owning actor. Never decoded
    from a script-selected ID, serialized or extended by runtime restoration.
    Captured current principal/source/generation/run revision is rechecked during
    final actor checkpoint/action commit. Lexical native handoff shadows None. *)
type t

val create
  :  run:Agent_protocol.Run.t
  -> execution_id:Agent_protocol.Id.Moderator_execution.t
  -> (t, Agent_protocol.Error.t) result

val run_id : t -> Agent_protocol.Id.Run.t
val principal_id : t -> Agent_protocol.Id.Principal.t
val source : t -> Agent_protocol.Run_source.t
val revision : t -> int64
val execution_id : t -> Agent_protocol.Id.Moderator_execution.t
val close : t -> unit
val is_open : t -> bool
