open! Core

(** Explicit process-local persistence selection. A name is a display/configuration
    selector, never authority to open a root or access its sessions. *)
module Root : sig
  type t

  val create : ?name:string -> path:string -> unit -> (t, Agent_protocol.Error.t) Result.t
  val path : t -> string
  val name : t -> string option
end

type t =
  | Default
  | Durable of Root.t
  | Transient

(** Pure resolution. Default requires an absolute supplied home and resolves to
    HOME/.ochat/agent-store. Explicit durable roots do not require HOME. Transient
    returns no root and must be allocated/owned by the embedded host. Never reads
    environment, derives cwd, creates directories or migrates prior storage. *)
val durable_root
  :  t
  -> home:string option
  -> (Root.t option, Agent_protocol.Error.t) Result.t
