open! Core

(** Explicit local host policy. Merely supplying a capacity file does not
    enroll a session in recovery capture. *)
val capacity_file : string option Command.Param.t

val load_capacity
  :  env:Eio_unix.Stdenv.base
  -> string option
  -> ( Agent_server.Recovery_capacity_configuration.t option
       , Agent_protocol.Error.t )
       result

val enable_flag : bool Command.Param.t

(** Use the supplied physical connection and its current owner attachment.
    The local host checks the selected session generation at the durable writer. *)
val enroll
  :  Agent_client.Connection.t
  -> Agent_protocol.Session.t
  -> attachment:Agent_protocol.Session.Attachment.t
  -> (unit, Agent_protocol.Error.t) result
