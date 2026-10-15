(** Eio workspace resolution without granting any read/write/tool capability. *)

val ownership_marker : string

(** Exact configured creation coordinate used by the existing resolver. Pure
    path calculation only; this neither resolves native identity nor grants
    access. Temporary roots require the actual configured managed root. *)
val planned_root
  :  Workspace_definition.t
  -> instance_id:Agent_protocol.Id.Workspace_instance.t
  -> session_directory:string
  -> string

(** Failed exclusive System_tmp creation removes only this call's successfully
    acquired directory, under protected cleanup. Preexisting paths are untouched.
    Ownership transfers only after the complete instance (including its timestamp)
    is returned. Session_dir remains owned by the session staging bracket.
    Original cancellation and unexpected exceptions propagate. *)
val resolve
  :  env:Eio_unix.Stdenv.base
  -> instance_id:Agent_protocol.Id.Workspace_instance.t
  -> session_directory:string
  -> Workspace_definition.t
  -> (Workspace_instance.t, Agent_store.Store_error.t) result

module Owned : sig
  (** Exact acquired native workspace cleanup lease, confined to the creating
      Eio domain and borrowed switch. No source/tool/workspace grant is conveyed. *)
  type t

  (** Switch-release retry reports an actual retained expected cleanup failure. *)
  exception Cleanup_failed of Agent_store.Store_error.t

  val instance : t -> Workspace_instance.t

  (** Infallible nonyielding cleanup-ownership release after genuine store
      publication evidence. Physical/Session_dir instances confer no removal
      authority. The caller must not transfer merely because its initializer
      returned: later layout publication can still fail. *)
  val transfer : t -> unit

  (** Protected idempotent removal of this exclusively acquired untransferred
      System_tmp instance through existing native marker/device/inode checks.
      Expected failure retains the actual lease for switch-release retry;
      unexpected cleanup exceptions retain their original backtrace for switch-release
      reporting as well as propagating. No active execution lease exists during
      creation. The borrowed switch must outlive creation and publication. *)
  val close : t -> (unit, Agent_store.Store_error.t) result
end

(** Preallocate ownership and switch-release cleanup before native acquisition;
    adopt the exact complete instance before the resolver releases its local
    acquisition bracket. [physical_roots] is the actual current configured
    physical workspace inventory used by existing cleanup protection. No
    preexisting directory is adopted. A trusted nonyielding, nonraising
    [cleanup_on_complete] is invoked once only after actual owned cleanup
    succeeds; developer construction uses it to release its exact provisional
    conflict-domain lease even if resolution fails before returning [Owned.t].
    Legacy [resolve] remains unchanged. *)
val resolve_owned
  :  ?cleanup_limits:Workspace_cleanup.Limits.t
  -> ?cleanup_checkpoint:(unit -> unit)
  -> ?cleanup_has_active_lease:(conflict_domain:string -> bool)
  -> ?cleanup_on_complete:(unit -> unit)
  -> sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> physical_roots:string list
  -> instance_id:Agent_protocol.Id.Workspace_instance.t
  -> session_directory:string
  -> Workspace_definition.t
  -> (Owned.t, Agent_store.Store_error.t) result

(** [resolve_current] captures the standalone process workspace explicitly. *)
val resolve_current
  :  env:Eio_unix.Stdenv.base
  -> instance_id:Agent_protocol.Id.Workspace_instance.t
  -> path:string
  -> access:Workspace_definition.access
  -> created_at:Agent_protocol.Timestamp.t
  -> (Workspace_instance.t, Agent_store.Store_error.t) result

(** [verify_available] checks that the captured device/inode identity has not
    silently changed since session creation. *)
val verify_available
  :  env:Eio_unix.Stdenv.base
  -> Workspace_instance.t
  -> (unit, Agent_store.Store_error.t) result

(** Rewrite only a genuine Session_dir instance to its final Store route. The
    caller supplies the actual Store publication destination; this operation
    canonicalizes its parent exactly as ordinary creation does and conveys no
    new source/workspace authority. Other admitted instance kinds are unchanged.
    Native canonicalization exceptions propagate. *)
val published_instance
  :  Workspace_instance.t
  -> final_directory:string
  -> Workspace_instance.t
