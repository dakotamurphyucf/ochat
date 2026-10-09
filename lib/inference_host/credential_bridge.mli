open! Core

(** Trusted runtime-host provider credential composition. Borrows the shared credential
    lifecycle authority; owns only a nonsecret projection on one Eio domain.
    No client credential forwarding, alternate provider client or ambient lookup. *)
module Compatible_profile = Compatible_profile

module Operation : sig
  type t =
    | Inference
    | Status
    | Configure
    | Remove
  [@@deriving equal, sexp_of]
end

module Error : sig
  type t =
    | Invalid_mapping
    | Invalid_environment
    | Invalid_credential
    | Missing_profile
    | Denied
    | Stale_authorization
    | Profile of Provider_profiles.Error.t
    | Lifecycle of Credential_registry.Error.t
    | Preparation of Inference_runtime.Preparation_error.t
  [@@deriving sexp_of]
end

module Mapping : sig
  type t

  (** Exact API-key or OAuth identity, including provider/billing/host/account and
      the grant-specific key reference or issuer/client/resource/subject tuple,
      is validated against the approved profile. Endpoint approval is a
      trusted host policy decision. The binding label names this complete host
      mapping; it is never secret material or a digest of secret material. *)
  val create
    :  Openai.Responses_driver.Profile.t
    -> revision:string
    -> binding:Credential_registry_model.Id.t
    -> identity:Credential_registry_model.Identity.t
    -> (t, Error.t) Result.t

  val profile : t -> string
  val binding : t -> Credential_registry_model.Id.t
end

module Environment : sig
  module Entry : sig
    type t

    (** Explicit host-selected source. Resolve may yield, but receives no arbitrary
        variable name: this entry has one declared binding/identity/name. Status is a non-yielding host snapshot; each returned resolved value
        owns its captured non-yielding source currentness guard. No serializer or secret
        diagnostic exists. Configuration revisions are host-issued opaque labels,
        never token hashes; absent revision disables authenticated WS reuse. *)
    val create
      :  binding:Credential_registry_model.Id.t
      -> identity:Credential_registry_model.Identity.t
      -> name:string
      -> configuration_revision:Credential_registry_model.Id.t option
      -> resolve:
           (sw:Eio.Switch.t
            -> (Credential_registry.Environment.resolved, Error.t) Result.t)
      -> status:(unit -> Credential_registry.Status.availability)
      -> (t, Error.t) Result.t
  end

  type t

  val create : Entry.t list -> (t, Error.t) Result.t

  (** Install only on the runtime host when opening the shared lifecycle. The
      resulting port resolves exact declared sources, without ambient fallback.
      Durable disabled metadata still wins when a variable remains present. *)
  val port : t -> Credential_registry.Environment.t
end

module Status : sig
  type availability =
    | Configured
    | Unavailable of Credential_registry.Status.availability
    | Disabled
  [@@deriving equal, sexp_of]

  type t

  val profile : t -> string
  val availability : t -> availability
  val lifecycle : t -> Credential_registry.Status.t option
end

module OAuth : sig
  type t

  (** Trusted66 qualified route port. No token decoding, login or fallback occurs
      in this bridge. It constructs only the exact route-specific lease from the
      admitted access material; existing lease guards and identity must survive. *)
  val create
    :  lease:
         (Credential_registry.Admission.t
          -> identity:Credential_registry_model.Identity.t
          -> profile:Openai.Responses_driver.Profile.t
          -> ( Openai.Responses_driver.Auth.lease
               , Openai.Responses_driver.Auth.error )
               Result.t)
    -> renewal:
         (Credential_registry_model.Identity.t -> Credential_registry.Renewal.t option)
    -> t

  (** The renewal selector is pure and non-yielding; it returns a qualified
      lifecycle port for the exact identity without acquiring credentials or
      initiating login. Expiry can remain dispatch-ready when this port exists;
      the lifecycle performs the actual authoritative refresh during admission.
      Uncertain renewal still requires explicit recovery. *)
end

type t

(** [authorize] is a trusted, non-yielding host check. It is also invoked under
    the lifecycle metadata lock at credential publication admission. Empty
    mappings permit an approved unknown-account OAuth template to bootstrap;
    inference remains unavailable until an exact verified mapping is published. *)

(** Compatible declarations have immutable IDs/revisions/defaults and reference
    canonical owners only. approved_profiles additionally reserves host-declared
    unmapped templates, allowing OAuth choices to remain unavailable until an
    exact verified mapping is published. All canonical and choice IDs are unique
    and bounded to128. Capture/resolve authorize both requested logical ID and
    owner ID; credential mutation APIs accept canonical IDs only. They share one
    lifecycle snapshot/epoch and renewal authority. Before publication, the
    canonical profile and all its explicitly declared choices receive one
    compatible replay policy. It preserves logical origin IDs, existing model
    declarations and driver capabilities; it grants neither authentication nor
    unknown native item compatibility. *)

val create
  :  ?oauth:OAuth.t
  -> ?compatible_profiles:Compatible_profile.t list
  -> ?approved_profiles:string list
  -> Openai.Responses_driver.t
  -> registry:Credential_registry.t
  -> mappings:Mapping.t list
  -> authorize:(principal:string -> profile:string -> operation:Operation.t -> bool)
  -> clock:_ Eio.Time.Mono.t
  -> maximum_wait:Time_ns.Span.t
  -> transport_policy:Inference.Observation.Transport_policy.t
  -> limits:Inference_runtime.Limits.t
  -> (t, Error.t) Result.t

(** Reload durable nonsecret state before fresh host resolution. Epoch changes
    update the existing62 registry; secret refresh revisions never change epochs.
    No synchronization or reauthorization occurs inside credential lookup. *)
val synchronize : t -> (unit, Error.t) Result.t

(** Trusted host administration over SAME owned entries. Mapping publication
    requires exact committed registry identity and unique profile/binding. All
    validation/read errors leave entries unchanged; replacement invalidates old
    contexts/plans before installing new configuration without yielding. Identical
    declared mapping is a no-op apart from metadata epoch synchronization. *)
val publish_mapping : t -> Mapping.t -> (unit, Error.t) Result.t

val mappings : t -> Mapping.t list

(** Pure driver-limit view sharing the exact credential authority and owned
    profile entries. Mutations remain visible to both; no new registry/store. *)
val with_response_limit : t -> max_body_bytes:int -> (t, Error.t) Result.t

(** current=None selects the explicit host default profile. A recapture supplies
    Some current and retains its exact approved binding/profile/account/endpoint;
    it never switches to the host default profile. *)
val capture
  :  t
  -> principal:string
  -> default_profile:string
  -> current:Inference.Request.Target.t option
  -> model:string
  -> settings:Openai.Responses_driver.Setting.t list
  -> (Inference.Request.Target.t, Error.t) Result.t

val resolve
  :  t
  -> principal:string
  -> Inference.Request.Target.t
  -> (Inference_runtime.Context.t, Error.t) Result.t

(** Typed conversion for the shared host preparation ports; detailed authorized
    lifecycle errors remain available on the administrative APIs. *)
val preparation_error : Error.t -> Inference_runtime.Preparation_error.t

val resolver : t -> principal:string -> Inference_runtime.resolver
val status : t -> principal:string -> profile:string -> (Status.t, Error.t) Result.t

(** Trusted local secure input only. Authorize before invoking read. Candidate
    failure preserves the working binding; a concurrent disable wins late CAS.
    API-key identities only; OAuth refuses before invoking read. No provider
    secret is accepted through an operator RPC or command argument.
    Optional trusted currentness guard is non-yielding and additive to the host
    authorization/mapping check after input validation and again under the final
    lifecycle metadata lock, after staging, before commit/publication admission.
    Filesystem publication already admitted may finish after expiry. False cancels the original candidate and preserves the old
    login. Omission retains explicit static-host composition behavior. *)
val enroll
  :  ?authorize_commit:(unit -> bool)
  -> t
  -> principal:string
  -> profile:string
  -> operation:Credential_registry_model.Id.t
  -> sw:Eio.Switch.t
  -> read:(sw:Eio.Switch.t -> (Provider_secret_store.Secret.t, Error.t) Result.t)
  -> (unit, Error.t) Result.t

(** Explicit declared environment reference; no key bytes copied to metadata.
    API-key identities only. No environment/protected-source fallback and no
    implicit initial provision. Optional non-yielding authorize_commit has the
    same additive pre-publication semantics as enroll. *)
val configure_environment
  :  ?authorize_commit:(unit -> bool)
  -> t
  -> principal:string
  -> profile:string
  -> operation:Credential_registry_model.Id.t
  -> name:string
  -> configuration_revision:Credential_registry_model.Id.t option
  -> (unit, Error.t) Result.t

(** Durable tombstone excludes future admissions, then bounded drain/cleanup.
    Existing admitted requests may finish. Environment removal disables the binding
    without claiming to erase the external variable. Unrelated profiles survive. *)
val remove
  :  t
  -> principal:string
  -> profile:string
  -> sw:Eio.Switch.t
  -> (Credential_registry.removal, Error.t) Result.t
