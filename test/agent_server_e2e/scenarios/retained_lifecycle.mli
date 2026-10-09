open! Core

(** Transport-independent actual archive/restore/resume journey. Each invocation
    owns a newly created stopped session and uses only the supplied RPC client. *)
type t [@@deriving equal, sexp_of]

val run
  :  request:
       (Agent_protocol.Command.t
        -> (Agent_protocol.Public.Result.t, Agent_protocol.Error.t) Result.t)
  -> created:Agent_protocol.Public.Result.Create.t
  -> key_prefix:string
  -> t

(** Reject any failed invariant, then compare normalized outcomes across transports. *)
val require_complete : t -> unit
