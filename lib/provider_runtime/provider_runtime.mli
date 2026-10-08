open! Core
module P = Agent_protocol
module Actor = Operator_authorization
module DTO = P.Provider_operator

(** One application-owned authority shared by operator commands and inference.
    Missing setup is a usable state, never implicit credential-store creation.
    Concrete provisioning supplies the same registry/bridge/profile owner to
    [Opened]; tests substitute only the trusted platform/acquisition ports. *)
module Opened : sig
  type t

  val create
    :  service:Provider_operator.t
    -> backend:Inference_host.Backend.t
    -> incarnation:DTO.Revision.t
    -> setup_receipt:
         (actor:Actor.t -> DTO.Setup_request.t -> (DTO.Revision.t, DTO.Error.t) Result.t)
    -> close:(unit -> unit)
    -> t

  val close : t -> unit
end

(** One Eio domain owns this state and all service workers. Request cancellation
    does not cancel host provisioning; [close] stops and joins it before borrowed
    storage can close. Request and daemon owners must call close before normal
    switch drain when an operator flow is pending. *)
type t

(** [existing] opens configured metadata without credential lookup/login; None
    means missing setup. Other startup failures remain typed. [initialize] is an
    explicit create-only, owner-bound idempotent provisioning operation. It must
    reconcile its original durable operation after publication ambiguity, never
    erase/reinitialize existing credentials. Failed callbacks join resources. *)
val create
  :  sw:Eio.Switch.t
  -> server_id:P.Id.Server.t
  -> authorize_setup:(Actor.t -> bool)
  -> authorize_status:(Actor.t -> bool)
  -> setup_receipt:
       (actor:Actor.t
        -> DTO.Setup_request.t
        -> (P.Command_receipt.t, DTO.Error.t) Result.t)
  -> existing:(sw:Eio.Switch.t -> (Opened.t option, DTO.Error.t) Result.t)
  -> initialize:
       (sw:Eio.Switch.t
        -> actor:Actor.t
        -> DTO.Setup_request.t
        -> (Opened.t, DTO.Error.t) Result.t)
  -> (t, DTO.Error.t) Result.t

(** Dynamic views share this state. Recapture is delegated with the captured
    target; setup/selection affects only future fresh captures. *)
val backend : t -> Inference_host.Backend.t

val dispatch
  :  t
  -> actor:Actor.t
  -> P.Command.t
  -> (P.Method_result.t, DTO.Error.t) Result.t

val receipt
  :  t
  -> actor:Actor.t
  -> P.Command.t
  -> (P.Command_receipt.t, DTO.Error.t) Result.t

(** Local-only protected-input enrollment; callback never travels over RPC. *)
val enroll_private_key
  :  t
  -> actor:Actor.t
  -> profile:DTO.Profile_id.t
  -> key:P.Idempotency_key.t
  -> source_reference:string
  -> sw:Eio.Switch.t
  -> read:
       (sw:Eio.Switch.t
        -> ( Provider_secret_store.Secret.t
             , Inference_host.Credential_bridge.Error.t )
             Result.t)
  -> (DTO.Configuration_result.t, DTO.Error.t) Result.t

val operator_port : t -> Agent_server.Provider_operator_port.t
val close : t -> unit
