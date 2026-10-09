(** Narrow root schema 1 -> 2 admission under the caller's exclusive daemon
    lock. Schema 2 requires organization authority; missing authority fails.
    Schema 1 installs/preserves authority first, then publishes schema 2 last.
    The caller retains [sw] and root ownership throughout the returned store. *)
val open_owned
  :  env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> root:Data_root.t
  -> server_id:Agent_protocol.Id.Server.t
  -> (Organization_store.t, Store_error.t) result

val create_owned
  :  env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> root:Data_root.t
  -> server_id:Agent_protocol.Id.Server.t
  -> created_at:Agent_protocol.Timestamp.t
  -> (Organization_store.t, Store_error.t) result

(** Read-only inspection; missing authority is admitted only for a legacy root
    whose envelope does not yet require it. Never initializes or replaces files. *)
val inspect_authority
  :  env:Eio_unix.Stdenv.base
  -> root:Data_root.t
  -> server_id:Agent_protocol.Id.Server.t
  -> required:bool
  -> (unit, Store_error.t) result
