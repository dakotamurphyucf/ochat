open! Core

(** Shared bounded Developer and semantic-authoring setup for real daemon E2E
    scenarios. The source policy selects only the authored document script; it
    does not grant native, provider, or transport authority. *)

val developer_configuration : unit -> Agent_server.Daemon.Developer_configuration.t

val semantic_authoring_policy
  :  Eio_unix.Stdenv.base
  -> Agent_server.Authoring_installation.Host_policy.t
