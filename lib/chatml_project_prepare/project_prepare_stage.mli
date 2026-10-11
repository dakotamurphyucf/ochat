open Core
module Attempt = Project_prepare_attempt
module Error = Project_prepare_error
module Build = Chatml_project_build
module Roots = Chatml_local_roots
module Git = Chatml_git_host
module Reader = Chatml_git_catalog.Reader

(** Host-owned accounting brackets around the actual existing backends. Reserve
    each stage's allowance before entry; nested stages see the remaining aggregate
    capacity after every outstanding reservation. Successful final receipts
    replace reservations exactly once. Failures abort the attempt; they do not
    refund capacity or authorize retries. Callback exceptions retain identity. *)

val pure
  :  Attempt.t
  -> f:
       (limits:Build.Limits.t
        -> checkpoint:(unit -> unit)
        -> ('a * Build.Accounting.t, Build.Error.t) Result.t)
  -> ('a, Error.t) Result.t

(** One resolver owns all retained local identities until its callback and joined
    cleanup finish. The callback must not return usable cursors/native owners.
    Root work remains reserved throughout nested package capture/composition. *)
val with_roots
  :  Attempt.t
  -> anchors:Roots.Anchor.t list
  -> f:(Roots.t -> ('a, Error.t) Result.t)
  -> ('a, Error.t) Result.t

(** Admit an exact host route before invoking the Git host. One repository scope
    contains one semantic Reader, so relative packages at this same repository/
    pin can share verified objects and the cumulative reader ledger. The callback
    must finish before another store acquisition begins; no reader/repository or
    mutable accounting may escape. Selected-lock policy is checked by the caller
    before entering this bracket. Cache publication does not publish a selection.

    Native request/output/work and payload reservations are independent of semantic
    object/tree/source/work reservations. No full aggregate allowance is reused
    independently by these overlapping scopes. Literal backend ceilings remain
    authoritative. An absent route/store rejects without transport or fallback. *)
val with_git
  :  Attempt.t
  -> host:Project_prepare_host.t
  -> repository:string
  -> pin:Git.Pin.t
  -> f:(Reader.t -> ('a, Error.t) Result.t)
  -> ('a, Error.t) Result.t
