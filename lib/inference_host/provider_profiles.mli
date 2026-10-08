open! Core

(** Runtime-host provider registry. Mutable state has one Eio-domain owner; callbacks
    may yield. No persistence, environment lookup, secret store or login is provided.
    Principal/profile/account/reference labels are nonsecret host identifiers.
    Captured Target/Selection remain the sole durable session intent. *)
module Error : sig
  type t =
    | Missing_profile
    | Denied
    | Incompatible_identity
    | Binding_unavailable
    | Disabled
    | Reauthorization_required
    | Invalid_profile
    | Preparation of Inference_runtime.Preparation_error.t
  [@@deriving equal, sexp_of]
end

module Profile : sig
  type t

  (** Only explicitly declared api_key or oauth_subscription methods are accepted.
      Subscription access is never an API-key fallback. credential_reference must
    identify the host's complete issuer/client-registration/audience/account tuple;
    generic OAuth tokens or API-key billing cannot be substituted for direct Codex
    subscription credentials. OAuth acquisition is future host lifecycle work. Revision records capture
      provenance of defaults; it is not credential generation or compatibility.
      Auth owner/generation are installed separately in the registry. *)
  val create
    :  Openai.Responses_driver.Profile.t
    -> revision:string
    -> binding:Inference.Request.Auth_binding.t
    -> (t, Error.t) Result.t

  val id : t -> string
end

module Credential_identity : sig
  type t

  val profile : t -> string
  val account : t -> string option
  val binding : t -> Inference.Request.Auth_binding.t
  val owner : t -> string
  val generation : t -> int64
  val equal : t -> t -> bool
  val sexp_of_t : t -> Sexp.t
end

module Status : sig
  type availability =
    | Available
    | Missing
    | Disabled
    | Reauthorization_required
  [@@deriving equal, sexp_of]

  type t

  val identity : t -> Credential_identity.t
  val availability : t -> availability
  val sexp_of_t : t -> Sexp.t
end

type t

(** Authorization and status callbacks are pure, non-yielding host policy snapshots;
    they run before credential callbacks. Registry mutation and admission execute on
    one Eio domain; native threads must deliver changes through that owner.
    Credential lookup may refresh the exact binding but must never initiate login,
    fall back modes/accounts, forward client secrets or return diagnostic secrets.
    Currentness is rechecked after lookup and before header publication. *)
val create
  :  Openai.Responses_driver.t
  -> authorize:
       (principal:string
        -> profile:string
        -> account:string option
        -> binding:Inference.Request.Auth_binding.t
        -> bool)
  -> credentials:
       (sw:Eio.Switch.t
        -> Credential_identity.t
        -> ( Openai.Responses_driver.Auth.lease
             , Openai.Responses_driver.Auth.error )
             Result.t)
  -> status:(Credential_identity.t -> Status.availability)
  -> limits:Inference_runtime.Limits.t
  -> t

(** Registry mutation is trusted host administration: the caller must authorize it
    independently of session-selected/imported data. Owner/generation are host
    lifecycle labels, never durable target fields. Reauthorization strictly advances
    generation; edits cancel already resolved contexts at dispatch (resolve the same
    captured Target again to use current capabilities without changing settings).
    Removal disables existing contexts, and profile IDs cannot be reused
    during this registry lifetime. Disabling does not erase an external environment
    variable or credential store. edit_profile changes defaults/capabilities/revision
    while preserving profile/account/endpoint/binding. Switch identity by installing
    a new profile ID and explicitly admitting Selection.change. *)
val add : t -> Profile.t -> owner:string -> generation:int64 -> (unit, Error.t) Result.t

val remove : t -> profile:string -> (unit, Error.t) Result.t
val edit_profile : t -> Profile.t -> (unit, Error.t) Result.t

val reauthorize
  :  t
  -> profile:string
  -> owner:string
  -> generation:int64
  -> (unit, Error.t) Result.t

val disable : t -> profile:string -> (unit, Error.t) Result.t

val capture
  :  t
  -> principal:string
  -> profile:string
  -> model:string
  -> settings:Openai.Responses_driver.Setting.t list
  -> (Inference.Request.Target.t, Error.t) Result.t

(** Missing/null historical binding is unavailable, never inferred. Captured revision
    and settings survive current-default edits. Compatibility is checked explicitly;
    preparation revalidates current capabilities and captures the current auth
    owner/generation independently for each plan, without credential access.
    Reauthorization permits a new plan on the same unchanged context, but old plans
    reject changed owner/generation before lookup. Silent refresh within the same
    generation remains permitted. Every dispatch rechecks principal authorization
    and resolves fresh auth for that plan's exact captured identity. *)
val resolve
  :  t
  -> principal:string
  -> Inference.Request.Target.t
  -> (Inference_runtime.Context.t, Error.t) Result.t

val resolver : t -> principal:string -> Inference_runtime.resolver
val status : t -> principal:string -> profile:string -> (Status.t, Error.t) Result.t
