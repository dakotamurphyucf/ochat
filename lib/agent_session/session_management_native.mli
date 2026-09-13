(** Native callback compatibility boundary. Resolves the actual expiring borrow
    and caller services, then delegates all argument decoding/operations to the
    transport-independent adapter. Tool admission and authorization occur before
    this callback, through the normal runtime dispatcher. *)
val run : Session_management.operation -> Jsonaf.t -> Agent_protocol.Invocation.outcome
