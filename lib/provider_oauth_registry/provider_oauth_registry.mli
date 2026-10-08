open! Core

(** 66 adapter only. Registry remains sole durable authority; no copied lifecycle
    implementation, implicit login, alternate route or cache import. *)
module Error : sig
  type t =
    | OAuth of Provider_oauth.Error.t
    | Registry of Credential_registry.Error.t
    | Invalid_binding
    | Denied
    | Closed
  [@@deriving sexp_of]
end

type t
type adapter = t

(** Borrowed fixed provider transport; caller closes it after all acquisitions and
    registry renewal operations have joined. No adapter-owned orphan work. *)
val create
  :  transport:Provider_oauth.Transport.t
  -> policy:Provider_oauth.Policy.t
  -> wall_clock:_ Eio.Time.clock
  -> t

(** Host chooses acquisition expectation, exact binding and original operation.
    Candidate begins before interactive work; current active login is preserved.
    start is a trusted host call to66 Login.start_browser/device using this sw.
    Return challenge only through authorized operator channel. Switch finalizer
    closes/joins login then cancels original pending candidate if not committed;
    ambiguous registry publication stays typed/recoverable, never retried under a
    new operation. No secret-bearing exception text is added. *)
module Acquisition : sig
  type t

  val start
    :  adapter
    -> registry:Credential_registry.t
    -> sw:Eio.Switch.t
    -> host:Credential_registry_model.Id.t
    -> binding:Credential_registry_model.Id.t
    -> operation:Credential_registry_model.Id.t
    -> expectation:Credential_registry_model.Expectation.t
    -> refresh_policy:Credential_registry_model.Grant.refresh_policy
    -> start:
         (sw:Eio.Switch.t
          -> ( Provider_oauth.Login.t * Provider_oauth.Challenge.t
               , Provider_oauth.Error.t )
               result)
    -> (t * Provider_oauth.Challenge.t, Error.t) result

  (** Single completion: joins66 result, constructs exact validated163 identity,
      grant and protected material, then checks [authorize_commit] and forwards it
      to final lifecycle metadata admission after secret staging. This trusted
      callback must not yield and must read current operator ownership/scopes.
      False returns [Denied] without activation and retires the original candidate;
      [close] remains safe. Filesystem publication already admitted may finish
      after authorization expiry. Cancellation
      preserves original exception/backtrace; protected cleanup never resubmits.
      Identity retains the expectation's exact configured required scopes;
      the verified provider grant independently covers them and retains all
      granted scopes/presence/provenance. Extra grants never redefine identity. *)
  val complete : t -> authorize_commit:(unit -> bool) -> (unit, Error.t) result

  val close : t -> (unit, Error.t) result
end

(** 63 callback port. Validates fixed issuer/client/resource/provider/billing,
    exact profile endpoint/account before accessing protected material. Returned
    lease composes Admission owner/epoch/currentness/revision;63 composes captured
    profile currentness without erasing this guard. No token claims parsed here. *)
val lease
  :  t
  -> Credential_registry.Admission.t
  -> identity:Credential_registry_model.Identity.t
  -> profile:Openai.Responses_driver.Profile.t
  -> (Openai.Responses_driver.Auth.lease, Openai.Responses_driver.Auth.error) result

(** Refresh callback restores bounded private continuity from supplied exact
    committed grant/material and performs fixed endpoint exchange in supplied sw.
    Registry already persists Possibly_sent. No retries, publication or fresh
    login here. Definitely_not_submitted only before dispatch ownership. *)
val renewal
  :  t
  -> Credential_registry_model.Identity.t
  -> Credential_registry.Renewal.t option
