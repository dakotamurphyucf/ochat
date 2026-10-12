open Core

(** Protection from the complete immutable session/archive inventory, before
    installing a loader or recovering actors. This is not execution authority. *)
module Limits : sig
  type t

  val create
    :  max_sessions:int
    -> max_archives:int
    -> max_jobs:int
    -> max_work_steps:int
    -> (t, Agent_protocol.Error.t) Result.t
end

module Receipt : sig
  type t

  val sessions : t -> int
  val archives : t -> int
  val jobs : t -> int
  val work_steps : t -> int
end

type t

val revisions : t -> Agent_protocol.Id.Prompt_revision.t list
val receipt : t -> Receipt.t

(** Bound the supplied entry prefix before retaining an accumulator. Borrow each
    exact indexed state through the Factory's original store/read owners; include
    archived sessions, every actual state source revision, and every retained
    Async_tool job regardless of terminal/disclosure status. Archive and job count
    admission precedes input-sized retention; JSON traversal polls and charges
    before the single Background_request codec is invoked. Absent selections keep
    v1 semantics. Present references retain validated exact revision IDs.

    One allowance covers session/archive/job counts and collector traversal work,
    including reference checks and 1024-byte JSON scalar chunks. Existing storage
    decoders own their per-file byte/structural ceilings; this receipt does not
    report their opaque internal work or pretend a shared storage-byte ceiling.
    No new default, source read, compiler, target or actor activation is added.

    Caller retains the genuine exclusive startup Store scope. Run after registry
    indexing/reader installation and before loader installation, recovery,
    connection publication or schedulers. Collect completely BEFORE entering the
    existing Delegation_store retention mutex. Then combine these revisions with
    its pending/parent artifact protection and catalog/baseline protection for the
    existing prune operation. Any missing/corrupt/untracked inventory, authority
    failure or exhausted allowance fails the WHOLE collection: skip ALL deletion,
    while ordinary startup continues to its existing recovery checks. Unexpected
    and checkpoint exceptions retain their original identity and backtrace. *)
val collect
  :  factory:Session_factory.t
  -> entries:Agent_store.Session_index.Entry.t list
  -> authorize:(Agent_protocol.Session.t -> (unit, Agent_protocol.Error.t) Result.t)
  -> request_policy:Chat_response.One_off_request.policy
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Agent_protocol.Error.t) Result.t
