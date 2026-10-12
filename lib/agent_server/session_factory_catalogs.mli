open Core

(** Private owner for the actual factory's workspace/current profile/history/grant
    catalogs. Mutations are confined to the creating Eio domain. Callbacks carried
    by already-compiled policies are borrowed values; no policy is evaluated here. *)
type t

val create
  :  workspaces:Agent_session.Workspace_catalog.t
  -> permission_profiles:Agent_session.Permission_policy.t list
  -> manifest_grants:Operator_manifest_grant.t list
  -> t

(** Readonly actual configured inventory; borrow under [with_admission] when
    deriving a native creation/cleanup route. No authorization is conveyed. *)
val workspace_definitions : t -> Agent_session.Workspace_definition.t list

val find_workspace
  :  t
  -> Agent_protocol.Id.Workspace_definition.t
  -> Agent_session.Workspace_definition.t option

val find_profile : t -> string -> Agent_session.Permission_policy.t option
val find_profile_revision : t -> string -> Agent_session.Permission_policy.t option
val manifest_grants : t -> Operator_manifest_grant.t list

module Admission : sig
  module Error : sig
    type t =
      | Busy
      | Publication_uncertain
      | Foreign_domain
    [@@deriving equal, sexp_of]
  end
end

(** Borrow the current publication generation on the creating Eio domain. Pending
    or uncertain publication rejects before [f]. Concurrent or nested admitted
    callbacks may suspend; no mutex is held. Their entire joined preparation must
    finish before returning. Normal return, exceptions and cancellation release
    the borrow without yielding; original exceptions/backtraces propagate.

    Any live borrow makes [seal] and [install] reject Busy before callbacks. The
    host must reserve this SAME owner before its durable writer and all coordinated
    prompt/config/service pointer swaps. This owner does not manage Prompt_catalog
    or grant authority. Pinned history reads and existing runtimes remain usable. *)
val with_admission : t -> f:(unit -> 'a) -> ('a, Admission.Error.t) Result.t

module Seal : sig
  module Error : sig
    type t =
      | Busy
      | Workspaces_busy
      | Publication_uncertain
      | Foreign_domain
      | Stale_preparation
      | Aborted
      | Committed
    [@@deriving equal, sexp_of]
  end

  (** SAME owner reservation, preallocated workspace seal + complete immutable
      profile/current/history/grant state and result carriers. No callback retained. *)
  type t

  (** Before acknowledged durability only. Releases both reservations without
      changing published pointers. Idempotent after abort; committed/uncertain reject. *)
  val abort : t -> (unit, Error.t) Result.t

  (** Old pointers/history remain intact; permanently refuses replacement until
      this owner is explicitly reconstructed. No choice of durable old/new bytes. *)
  val mark_uncertain : t -> (unit, Error.t) Result.t

  (** AFTER acknowledged durability: same-domain, no locks, polls, callbacks, IO,
      fresh allocation or yielding; only precomputed workspace/state/status pointer
      assignments. Idempotent after commit; aborted/uncertain reject. Root coordinates
      Prompt_catalog.Seal and all other services in the SAME protected final phase. *)
  val commit : t -> (unit, Error.t) Result.t
end

(** Preallocate current profiles, extended pinned revision history, grants and a
    Workspace_catalog seal BEFORE durable publication. Scans/polls/allocations occur
    here; legacy replacements cannot invalidate an admitted reservation. Original
    checkpoint exceptions/cancellation release any temporary workspace reservation,
    preserve all published cells and re-raise original exception/backtrace.
    Profile insertion semantics stay the existing last-definition-wins semantics;
    validated Config is still the authoritative duplicate/permission admission owner.
    Work is proportional to supplied profiles, logarithmic in retained map sizes.
    Existing immutable workspace indexes are reused; no workspace native IO occurs.
    Live [with_admission] callbacks reject Busy before the first checkpoint and are
    checked again after preallocation, before the final reservation. *)
val seal
  :  t
  -> workspaces:Agent_session.Workspace_catalog.t
  -> permission_profiles:Agent_session.Permission_policy.t list
  -> manifest_grants:Operator_manifest_grant.t list
  -> checkpoint:(unit -> unit)
  -> (Seal.t, Seal.Error.t) Result.t

(** Compatibility installer uses the SAME preallocation/reservation/commit path.
    Raises [Invalid_argument] on busy/uncertain/foreign-domain admission, BEFORE any
    published cell changes; exception is outside any mutable mutex. *)
val install
  :  t
  -> workspaces:Agent_session.Workspace_catalog.t
  -> permission_profiles:Agent_session.Permission_policy.t list
  -> manifest_grants:Operator_manifest_grant.t list
  -> unit
