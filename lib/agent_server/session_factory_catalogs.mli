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

val find_workspace
  :  t
  -> Agent_protocol.Id.Workspace_definition.t
  -> Agent_session.Workspace_definition.t option

val find_profile : t -> string -> Agent_session.Permission_policy.t option
val find_profile_revision : t -> string -> Agent_session.Permission_policy.t option
val manifest_grants : t -> Operator_manifest_grant.t list

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
    Existing immutable workspace indexes are reused; no workspace native IO occurs. *)
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
