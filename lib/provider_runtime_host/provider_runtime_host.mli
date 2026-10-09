module Profile_policy : module type of Profile_policy
open! Core
module P = Agent_protocol
module Actor = Operator_authorization
module DTO = P.Provider_operator
module M = Credential_registry_model
module Bridge = Inference_host.Credential_bridge

(** Actual reusable production composition. The same owned registry, bridge,
    approved templates, operation proofs and service back both operator commands
    and inference. Callers inject declared configuration and qualified66 login;
    no caller-specific registry, token decoder or implicit enrollment is created. *)
val create
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> server_id:P.Id.Server.t
  -> anchor:Eio.Fs.dir_ty Eio.Path.t
  -> components:Private_storage.Name.t list
  -> host:M.Id.t
  -> secret_namespace:Provider_secret_store.Namespace.t
  -> driver:Openai.Responses_driver.t
  -> templates:Provider_operator.Profile_admin.Template.t list
  -> mappings:Bridge.Mapping.t list
  -> compatible_profiles:Bridge.Compatible_profile.t list
  -> default_profile:DTO.Profile_id.t
  -> environment:Bridge.Environment.t option
  -> environment_sources:Provider_operator.Environment_source.t list
  -> oauth:Provider_oauth_registry.t
  -> oauth_lease:Bridge.OAuth.t option
  -> start_login:
       (sw:Eio.Switch.t
        -> template:Provider_operator.Profile_admin.Template.t
        -> mode:DTO.Login_mode.t
        -> ( Provider_oauth.Login.t * Provider_oauth.Challenge.t
             , Provider_oauth.Error.t )
             Result.t)
  -> inference_principal:string
  -> authorize_bridge:
       (principal:string -> profile:string -> operation:Bridge.Operation.t -> bool)
  -> authorize:(Actor.t -> operation:DTO.Operation.t -> profile:DTO.Profile_id.t -> bool)
  -> authorize_setup:(Actor.t -> bool)
  -> authorize_status:(Actor.t -> bool)
  -> new_operation:(unit -> M.Id.t)
  -> new_revision:(unit -> DTO.Revision.t)
  -> maximum_wait:Time_ns.Span.t
  -> limits:DTO.Limits.t
  -> inference_limits:Inference_runtime.Limits.t
  -> transport_policy:Inference.Observation.Transport_policy.t
  -> (Provider_runtime.t, DTO.Error.t) Result.t
