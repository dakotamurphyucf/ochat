(** Typed request connection with one serialized transport owner. *)

type t

val create : Transport.t -> t

val request
  :  t
  -> Agent_protocol.Command.t
  -> (Agent_protocol.Public.Result.t, Agent_protocol.Error.t) result

(** Accepts only the public Non_history response whitelist. Excludes the inline
    snapshot/history containers in get/create/attach, not authority or disclosure:
    export can still reference a history artifact. *)
val request_without_history
  :  t
  -> Agent_protocol.Command.t
  -> (Agent_protocol.Method_result.t, Agent_protocol.Error.t) result

(** The last validated successful initialize response on this transport. Failed
    initialization and unrelated responses do not establish support; closed
    connections expose None. Capability selection does not grant authority. *)
val initialization : t -> Agent_protocol.Initialize.Response.t option

val next_notification : t -> Agent_protocol.Envelope.t option
val close : t -> unit
