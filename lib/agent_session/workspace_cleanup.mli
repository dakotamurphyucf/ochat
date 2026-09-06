(** Guarded cleanup for server-created temporary workspace instances. *)

type event =
  | Session_stop
  | Session_delete
[@@deriving compare, equal, sexp]

type protected_roots =
  { data_root : string
  ; physical_workspaces : string list
  ; managed_roots : string list
  }

(** [cleanup] removes only the exact path captured in a temporary instance,
    after checking cleanup policy, managed-root containment, protected roots,
    and active leases. *)
val cleanup
  :  env:Eio_unix.Stdenv.base
  -> protected_roots:protected_roots
  -> expected_path:string
  -> has_active_lease:(conflict_domain:string -> bool)
  -> event:event
  -> now:Agent_protocol.Timestamp.t
  -> Workspace_instance.t
  -> (Workspace_instance.t, Agent_store.Store_error.t) result

(** [remove] removes a verified server-created temporary workspace regardless
    of its automatic cleanup policy. Administrative reset uses this operation
    after the session has stopped and released its workspace lease. *)
val remove
  :  env:Eio_unix.Stdenv.base
  -> protected_roots:protected_roots
  -> expected_path:string
  -> has_active_lease:(conflict_domain:string -> bool)
  -> now:Agent_protocol.Timestamp.t
  -> Workspace_instance.t
  -> (Workspace_instance.t, Agent_store.Store_error.t) result
