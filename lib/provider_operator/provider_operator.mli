open! Core
module P = Agent_protocol
module Actor = Operator_authorization
module DTO = P.Provider_operator
module M = Credential_registry_model
module Bridge = Inference_host.Credential_bridge
module Owner_records : module type of Owner_records
module Command_intents : module type of Command_intents
module Profile_admin : module type of Profile_admin

(** Host-owned login/status/cancel/logout/selection service. All authentication,
    candidate publication and refresh remain in66/163; no token decoding here. *)
type t

module Environment_source : sig
  type t

  val create
    :  id:DTO.Source_id.t
    -> name:string
    -> revision:M.Id.t option
    -> (t, DTO.Error.t) Result.t
end

(** Caller host switch owns workers independently of request/connection/session
    switches. Current authorization is rechecked on every call and immediately
    before66 commits. start_login is an explicitly configured qualified66 port;
    it cannot choose caller-supplied routes or credentials. now is host wall time,
    used for the original public flow expiry at challenge disclosure and final
    commit admission;66 additionally owns monotonic acquisition deadlines. *)
val create
  :  sw:Eio.Switch.t
  -> server_id:P.Id.Server.t
  -> host:M.Id.t
  -> incarnation:M.Id.t
  -> registry:Credential_registry.t
  -> bridge:Bridge.t
  -> profiles:Profile_admin.t
  -> oauth:Provider_oauth_registry.t
  -> owner_records:Owner_records.t
  -> command_intents:Command_intents.t
  -> start_login:
       (sw:Eio.Switch.t
        -> template:Profile_admin.Template.t
        -> mode:DTO.Login_mode.t
        -> ( Provider_oauth.Login.t * Provider_oauth.Challenge.t
             , Provider_oauth.Error.t )
             Result.t)
  -> authorize:(Actor.t -> operation:DTO.Operation.t -> profile:DTO.Profile_id.t -> bool)
  -> clock:_ Eio.Time.Mono.t
  -> maximum_wait:Time_ns.Span.t
  -> now:(unit -> P.Timestamp.t)
  -> new_operation:(unit -> M.Id.t)
  -> limits:DTO.Limits.t
  -> environment:Environment_source.t list
  -> (t, DTO.Error.t) Result.t

val status
  :  t
  -> actor:Actor.t
  -> DTO.Status_request.t
  -> (DTO.Status_result.t, DTO.Error.t) Result.t

val begin_login
  :  t
  -> actor:Actor.t
  -> DTO.Login_request.t
  -> (DTO.Flow_ref.t, DTO.Error.t) Result.t

val challenge
  :  t
  -> actor:Actor.t
  -> flow:DTO.Flow_ref.t
  -> (DTO.Private_challenge.t, DTO.Error.t) Result.t

val cancel
  :  t
  -> actor:Actor.t
  -> DTO.Cancel_request.t
  -> (DTO.Flow_result.t, DTO.Error.t) Result.t

val logout
  :  t
  -> actor:Actor.t
  -> sw:Eio.Switch.t
  -> DTO.Logout_request.t
  -> (DTO.Logout_result.t, DTO.Error.t) Result.t

val select
  :  t
  -> actor:Actor.t
  -> DTO.Select_request.t
  -> (DTO.Selection_result.t, DTO.Error.t) Result.t

val configure_environment
  :  t
  -> actor:Actor.t
  -> DTO.Environment_request.t
  -> (DTO.Configuration_result.t, DTO.Error.t) Result.t

(** LOCAL protected-input path only; never command parameters/argv/RPC. Authorize
    before input callback; bridge preserves prior key until two-phase publication. *)
val enroll_private_key
  :  t
  -> actor:Actor.t
  -> profile:DTO.Profile_id.t
  -> key:P.Idempotency_key.t
  -> source_reference:string
  -> sw:Eio.Switch.t
  -> read:(sw:Eio.Switch.t -> (Provider_secret_store.Secret.t, Bridge.Error.t) Result.t)
  -> (DTO.Configuration_result.t, DTO.Error.t) Result.t

(** Original owner-bound login receipt/recovery. Foreign principal cannot take
    over a flow after restart; interrupted browser/device acquisition is never
    restarted. Returns only nonsecret state. *)
val login_receipt
  :  t
  -> actor:Actor.t
  -> key:P.Idempotency_key.t
  -> (DTO.Flow_result.t option, DTO.Error.t) Result.t

val close : t -> unit

(** Read-only reconciliation of original provider command under current scopes;
    never reruns a missing/uncertain effect or serializes a challenge. *)
val command_receipt
  :  t
  -> actor:Actor.t
  -> P.Command.t
  -> (P.Command_receipt.t, DTO.Error.t) Result.t
