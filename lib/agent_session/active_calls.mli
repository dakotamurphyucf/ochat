(** Ephemeral active-call summaries, owned and synchronized by the session actor.
    Retain at most 1024 start envelopes, with invocation payload fields bounded
    to 4096 bytes. Preserve call identities exactly.
    Agent calls are the classified subset. Terminal events remove summaries;
    no executable continuation or completed-call history is retained. *)
type t

val create : unit -> t
val observe : t -> Agent_protocol.Event.Recoverable.t -> unit
val finish : t -> Agent_protocol.Event.Durable.t -> unit
val snapshot : t -> Jsonaf.t list * Jsonaf.t list
