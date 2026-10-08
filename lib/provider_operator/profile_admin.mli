open! Core
module P = Agent_protocol
module DTO = P.Provider_operator
module M = Credential_registry_model
module Bridge = Inference_host.Credential_bridge

module Error : sig
  type t =
    | Invalid_template
    | Missing_profile
    | Conflict
    | Missing_setup
    | Busy
    | Publication_uncertain
    | Storage of Private_storage.Error.t
    | Registry of Credential_registry.Error.t
    | Bridge of Bridge.Error.t
  [@@deriving sexp_of]
end

module Template : sig
  type authentication =
    | Api_key
    | Direct_codex
  [@@deriving equal, sexp_of]

  type t

  (** Trusted host declaration. mapping is a pure exact route/profile builder;
      verified identity must satisfy expectation before it is invoked. Persisted
      descriptor binds this profile/binding/revision and optional expected account;
      changing declaration requires explicit configuration migration. *)
  val create
    :  profile:DTO.Profile_id.t
    -> binding:M.Id.t
    -> revision:DTO.Revision.t
    -> authentication:authentication
    -> expectation:M.Expectation.t
    -> expected_account:string option
    -> mapping:(M.Identity.t -> (Bridge.Mapping.t, Bridge.Error.t) Result.t)
    -> (t, Error.t) Result.t

  val profile : t -> DTO.Profile_id.t
  val binding : t -> M.Id.t
  val expectation : t -> M.Expectation.t
  val authentication : t -> authentication
end

(** Bounded atomic committed selection evidence. Oldest completed proof is
    evicted after 64 later retained operations; absence proves no outcome. *)
module Selection_proofs : sig
  type t

  val maximum : int
  val empty : t
  val length : t -> int

  val lookup
    :  t
    -> principal:P.Id.Principal.t
    -> operation:M.Id.t
    -> DTO.Select_request.t
    -> (DTO.Selection_result.t option, Error.t) Result.t

  val add
    :  t
    -> principal:P.Id.Principal.t
    -> operation:M.Id.t
    -> DTO.Select_request.t
    -> DTO.Selection_result.t
    -> (t, Error.t) Result.t

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

type t

(** Trusted setup only, create-only nonsecret approved templates/selection. *)
val initialize
  :  Private_storage.Directory.t
  -> incarnation:M.Id.t
  -> templates:Template.t list
  -> default_profile:DTO.Profile_id.t
  -> initial_revision:DTO.Revision.t
  -> (unit, Error.t) Result.t

(** Borrows same credential registry/bridge. publish is the bridge's trusted
    exact-entry replacement operation, not an alternate profile table. Existing
    metadata must exactly match configured template descriptors/incarnation. *)
val open_
  :  Private_storage.Directory.t
  -> incarnation:M.Id.t
  -> registry:Credential_registry.t
  -> templates:Template.t list
  -> publish:(Bridge.Mapping.t -> (unit, Bridge.Error.t) Result.t)
  -> new_revision:(unit -> DTO.Revision.t)
  -> (t, Error.t) Result.t

val templates : t -> Template.t list
val find_template : t -> DTO.Profile_id.t -> (Template.t, Error.t) Result.t
val synchronize : t -> (unit, Error.t) Result.t

(** Reconcile original committed operation then publish exact authoritative
    identity. A credential-commit/profile-publication crash recovers by synchronize;
    pending/uncertain original operation is never presumed committed. *)
val publish_committed
  :  t
  -> template:Template.t
  -> operation:M.Id.t
  -> (unit, Error.t) Result.t

val selection : t -> (DTO.Selection_result.t, Error.t) Result.t

(** Publishes selection and exact principal/request/operation proof atomically.
    Reconciliation returns the original result within the 64-proof window,
    independent of current selection. Outside it Publication_uncertain forbids
    replay; unresolved command intents are not erased. Legacy version1 metadata
    remains readable but does not fabricate exact request evidence. *)
val select
  :  t
  -> principal:P.Id.Principal.t
  -> operation:M.Id.t
  -> reconcile:bool
  -> DTO.Select_request.t
  -> (DTO.Selection_result.t, Error.t) Result.t

(** Actual runtime composition: only current=None reads persisted selection.
    Recapture keeps current target identity. Bounded views borrow the same
    selection authority and shared bridge entries. *)
val backend : t -> bridge:Bridge.t -> principal:string -> Inference_host.Backend.t
