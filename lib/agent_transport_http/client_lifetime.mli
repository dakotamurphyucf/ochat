(** Own all HTTP-client fibers and resources in a connection-local Eio switch. *)
type t

val switch : t -> Eio.Switch.t

(** [start ~sw construct] initializes a connection in a child switch. Failed
    initialization releases partial resources before returning. Cancelling the
    initialization caller also cancels and joins partial resources, even when
    [sw] remains alive. Successful
    connections must call [close]; parent cancellation also releases them. *)
val start
  :  sw:Eio.Switch.t
  -> (t -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** [close t] cancels and joins every connection-owned fiber, including response
    monitors that outlive transport shutdown. Call from outside the owned switch.
    Cleanup is cancellation-protected. Repeated calls are harmless. *)
val close : t -> unit
