(** Eio workspace resolution without granting any read/write/tool capability. *)

val ownership_marker : string

val resolve
  :  env:Eio_unix.Stdenv.base
  -> instance_id:Agent_protocol.Id.Workspace_instance.t
  -> session_directory:string
  -> Workspace_definition.t
  -> (Workspace_instance.t, Agent_store.Store_error.t) result

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
