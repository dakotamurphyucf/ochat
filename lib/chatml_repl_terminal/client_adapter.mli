open! Core

(** Physical local/remote connection bracket for the same portable client.
    This module installs no service, source grant or permission. *)

module Local : sig
  type t

  type storage =
    | Durable of string
    | Transient

  (** [executable] is the trusted [ochat-agent-stdio] binary. [policy_file]
      selects the explicit local Developer/Authoring setup; prompt and
      workspace are existing host paths, not implicit grants. *)
  val create
    :  executable:string
    -> policy_file:string
    -> prompt_file:string
    -> workspace:string
    -> storage:storage
    -> (t, Agent_protocol.Error.t) result
end

type target =
  | Local of Local.t
  | Remote of Agent_transport_client.Connection_profile.t

type t

(** Local uses the Eio-owned stdio child and performs physical Initialize.
    Remote uses Connection_profile's initialized HTTP/Unix connection and
    pinned server identity. Both require the actually selected REPL feature
    and installed capabilities before [f] receives a session. The connection
    and any child are joined/closed on all exits; retaining [t] cannot extend
    its lifetime. Callback error/cancellation retains priority over cleanup. *)
val with_connection
  :  env:Eio_unix.Stdenv.base
  -> target:target
  -> notification_capacity:int
  -> f:(t -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

val client : t -> Agent_client.Developer_repl_connection.t
val connection : t -> Agent_client.Connection.t
val description : t -> string
