(** Injected borrowed host authority for the owning actor's membership commit.
    Server composition captures the root-owned store and exact host identity.
    This module does not import server/store or acquire actor lifetime. *)
type t

(** The supplied implementation checks current availability, host, manage scope,
    and live owner/admin-visible additions while holding the organization mutex.
    It invokes [commit] at most once. [commit] may yield only in the owning actor's
    persistence operation; it cannot send/wait actor messages, reenter organization
    authority, activate runtime, or call event/index/subscriber hooks. Release the
    mutex before returning/rethrowing. Preserve cancellation and its backtrace. *)
val create
  :  (principal:Agent_protocol.Principal.t
      -> host_id:Agent_protocol.Id.Server.t
      -> additions:Agent_protocol.Session_organization.Values.t
      -> commit:(unit -> (unit, Agent_protocol.Error.t) result)
      -> (unit, Agent_protocol.Error.t) result)
  -> t

(** Caller is already in its actor turn with current writer validation and a
    prepared transition. Only successful persistence permits subsequent in-memory
    installation/event/index hooks, which run after this operation releases the
    organization lock. Caller protects admission-through-acknowledged canonical
    installation and post-unlock publication with bounded [Eio.Cancel.protect]; pending cancellation at mutex
    exit cannot leave actor memory/receipt behind an acknowledged journal commit.
    Event/index hooks follow unlock and canonical installation while protected.
    Callback failures after acknowledgement do not undo persisted state or memory;
    they terminate the owning actor lifetime and require durable reconciliation.
    Exceptions before
    acknowledged commit preserve original uncertainty/backtrace. Never acquire
    this capability while holding projection/index/writer locks. *)
val persist
  :  t
  -> principal:Agent_protocol.Principal.t
  -> host_id:Agent_protocol.Id.Server.t
  -> additions:Agent_protocol.Session_organization.Values.t
  -> commit:(unit -> (unit, Agent_protocol.Error.t) result)
  -> (unit, Agent_protocol.Error.t) result
