type t

(** Real embedded Headless owner, one unbootstrapped connection and one subscribed
    handle. No native owner escapes this actual host scope. *)
val with_run : mode:Agent_protocol.Run.Mode.t -> startup:string -> f:(t -> unit) -> unit

val admission : t -> Agent_protocol.Run_receipt.t
val requests : t -> int
val snapshot : t -> Agent_protocol.Public.Snapshot.Fields.t

(** Wait on the real Run_views feed and reader closure. No Idle-as-terminal or
    ad hoc event-summary wakeup. *)
val await
  :  t
  -> f:(Agent_protocol.Run_query.View.t -> bool)
  -> Agent_protocol.Run_query.View.t

val query : t -> Agent_protocol.Run_query.View.t
