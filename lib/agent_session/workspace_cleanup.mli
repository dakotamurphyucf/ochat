open! Core

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

module Limits : sig
  type t

  (** Finite directory entries, retained name bytes and traversal steps for
      EACH cleanup attempt. The immutable policy is reused on retry, which
      receives a fresh finite allowance; exact partially removed state and its
      owner are retained. Existing conversation cleanup retains its policy. *)
  val create : max_entries:int -> max_bytes:int -> max_work_steps:int -> t Or_error.t

  (** Same validated finite policy for a provisional owner-created tree. *)
  val tree : t -> Agent_store.Bounded_tree_cleanup.Limits.t
end

module Progress : sig
  type t

  (** Initial state only. The cleanup implementation binds this receipt to a
      verified root and advances it after unlinking the exact marker. Retain it
      with the owner for same-process retries; it grants no root authority. *)
  val create : unit -> t
end

(** Developer Live variant of [remove]. Check the same protected paths,
    marker, device/inode and absent OTHER active lease. The caller retains its
    exact cleanup lease through deletion and supplies
    [Workspace_lease.active_other_than] to [has_active_lease]. A bounded no-follow
    directory stream removes entries in postorder, checking cancellation
    between chunks; it never performs a bounded prewalk followed by an
    unbounded [rmtree]. An inner symbolic link is unlinked as a link, never
    followed; linked root or ownership marker rejects. A failed attempt retains
    the exact owner and partial state for retry. Final marker removal and root
    rmdir are a protected tail; failure after removing the marker retains the
    in-memory root identity for a same-process retry. After restart an
    unmarked leftover is uncertain and quarantined, never treated as absent or
    silently adopted. Never report completion from a partial tree. Ordinary
    session cleanup remains unchanged. *)
val remove_bounded
  :  env:Eio_unix.Stdenv.base
  -> protected_roots:protected_roots
  -> expected_path:string
  -> has_active_lease:(conflict_domain:string -> bool)
  -> now:Agent_protocol.Timestamp.t
  -> limits:Limits.t
  -> progress:Progress.t
  -> checkpoint:(unit -> unit)
  -> Workspace_instance.t
  -> (Workspace_instance.t, Agent_store.Store_error.t) result

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
