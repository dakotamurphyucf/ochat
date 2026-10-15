open! Core

(** One provisional/Live developer workspace lifetime. It reuses the daemon's
    actual Workspace_lease ledger, Workspace_resolver and Store-owned ephemeral
    directory. It grants no Root, source read, native tool or provider effect. *)
type t

module Error : sig
  type t =
    | Store of Agent_store.Developer_run_store.Error.t
    | Workspace of Agent_store.Store_error.t
    | Lease of Agent_protocol.Error.t
    | Unsupported_cleanup of Agent_session.Workspace_definition.cleanup
    | Foreign_or_stale
  [@@deriving sexp_of]
end

(** Validate the configured cleanup mode BEFORE opening a directory or
    resolving a native root. Physical definitions never acquire removal
    authority. Temporary On_session_stop is interpreted as cleanup when this
    developer Live owner stops after joined work; Temporary On_session_delete
    and Retain have no developer-run deletion/retention lifecycle and return
    Unsupported_cleanup without filesystem work. Conversation semantics are
    unchanged.

    The caller supplies a genuine current configured definition and exact
    Store handle; it must separately authorize the original/current actor,
    prompt/workspace pair and source. [open_live] obtains one Store-issued
    exclusive ephemeral directory, calls Workspace_resolver.resolve_owned,
    then acquires THIS daemon's shared workspace lease using the Store owner.
    Failed/cancelled acquisition cleans only its own resources in reverse
    order, retaining typed cleanup custody on an uncertain failure. The same
    finite [cleanup_limits] policy is retained for switch-release retries, each
    of which receives a fresh bounded allowance; checkpoint
    exceptions/cancellation keep their identity. No Root/command publication
    or native resource initialization occurs here. *)
val open_live
  :  env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> handle:Agent_store.Developer_run_store.handle
  -> definition:Agent_session.Workspace_definition.t
  -> workspace_leases:Agent_session.Workspace_lease.t
  -> physical_roots:string list
  -> cleanup_limits:Agent_session.Workspace_cleanup.Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) result

val instance : t -> Agent_session.Workspace_instance.t
val conflict_domain : t -> string

(** Actual Store directory/lease and device/inode must remain live. This is
    comparison/currentness only; it never replaces actor, prompt or native
    selection checks at an effect boundary. *)
val check_live : t -> (unit, Error.t) result

(** Actual owner-derived Runtime_paths coordinates. The caller must keep [t]
    live through all Ctx/native/Model borrowers, and may not infer authority
    from these strings. *)
val runtime_directory : t -> string

val cache_directory : t -> string
val response_directory : t -> string

(** Called after native/job borrowers have joined. Keep the exact lease held
    while bounded cleanup removes an On_session_stop temporary workspace; the
    cleanup callback ignores only this exact token and observes any other
    holder. Release the lease only after successful workspace removal, then
    close the ephemeral directory. Physical roots are never removed. Partial
    deletion or cancellation retains the lease and Store child slot
    for retry; if final marker removal precedes a failed rmdir, the same-process
    owner retains exact root identity, while startup quarantines an unmarked
    leftover. A failed close never reports success or frees the daemon lock.
    Repeated close after success is harmless. *)
val close_and_wait : t -> (unit, Error.t) result
