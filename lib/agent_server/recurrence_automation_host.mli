open! Core

(** One actual daemon instance's administrative planning policy and configured
    timezone owner. This is not target/creation authority or stored credentials. *)
type t

module Zone_status : sig
  type t =
    | Not_configured
    | Directory_unavailable
    | Available
  [@@deriving equal, sexp_of]
end

(** Validate all configuration before IO. Open only the explicit absolute path
    from supplied fs: acquire its filesystem-root capability, then use existing
    Local.open_subroot for EVERY configured component with O_NOFOLLOW. Directory
    aliases and zone-file aliases are unsupported. No ambient TZ/database/path
    fallback, new FFI, cache or resolver exists. Invalid configuration rejects;
    expected directory failure retains interval grants with unavailable calendar
    data. Cancellation/checkpoint exceptions propagate with joined cleanup.

    Caller keeps sw alive through close_and_wait. The owner is used in its
    creating Eio domain. A fresh process reads current config and issues new
    original guarded proofs; controller/creator IDs never restore authority. *)
val open_owned
  :  Recurrence_automation_configuration.t
  -> server_id:Agent_protocol.Id.Server.t
  -> sw:Eio.Switch.t
  -> fs:Eio.Fs.dir_ty Eio.Path.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> checkpoint:(unit -> unit)
  -> (t, Agent_protocol.Error.t) result

val planning_host : t -> Agent_session.Recurrence_planning_host.t
val zone_status : t -> Zone_status.t

(** Nonblocking revocation first. The daemon joins/cancels scheduler sweep
    fibers, then close_and_wait joins all borrowed loads and releases this root.
    Supplied switch remains the final joined owner on exceptional exits. *)
val close : t -> unit

val close_and_wait : t -> unit
