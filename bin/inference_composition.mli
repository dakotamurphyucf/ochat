module Provider_commands : module type of Provider_commands
open! Core

(** Pure conversion of the existing CLI API_URL convention to a Responses
    endpoint. Profile construction performs endpoint validation. *)
val responses_endpoint : api_url:string option -> string

module Error : sig
  type t =
    | Invalid_configuration
    | Setup_required
    | Provider of Inference_host.Provider_configuration.Error.t
    | Preparation of Inference_runtime.Preparation_error.t
  [@@deriving sexp_of]
end

module Configuration : sig
  type t

  (** Explicit runtime-host sources and authority, including private-file setup
      mode. No source precedence fallback or automatic enrollment occurs. *)
  val create
    :  storage:Inference_host.Provider_configuration.t
    -> mappings:Inference_host.Credential_bridge.Mapping.t list
    -> default_profile:string
    -> environment:Inference_host.Credential_bridge.Environment.t option
    -> oauth:Inference_host.Credential_bridge.OAuth.t option
    -> principal:string
    -> authorize:
         (principal:string
          -> profile:string
          -> operation:Inference_host.Credential_bridge.Operation.t
          -> bool)
    -> transport_policy:Inference.Observation.Transport_policy.t
    -> (t, Error.t) Result.t

  (** Trusted CLI boundary only. Selects the declared host variable and endpoint;
      lookup is injected and never imported into session/runtime state. Storage is
      outside session roots. Existing is the ordinary entrypoint mode; Initialize
      is an explicit setup override and still does not enroll the source. *)
  val of_environment
    :  env:Eio_unix.Stdenv.base
    -> home:string
    -> api_url:string option
    -> key_name:string
    -> lookup:(string -> string option)
    -> mode:Inference_host.Provider_configuration.Mode.t
    -> (t, Error.t) Result.t
end

module Opened : sig
  type t

  val host : t -> Inference_host.t
  val bridge : t -> Inference_host.Credential_bridge.t
end

(** Explicit setup/operator composition retains the trusted local bridge for
    authorized status/configure/remove operations. No public credential DTO. *)
val try_open
  :  Configuration.t
  -> sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> default_model:string
  -> (Opened.t, Error.t) Result.t

(** Opening does not resolve credentials or disable unrelated read-only operator
    commands. Caller switch owns provider resources through all graph/attempt
    lifetimes. Detailed expected failures remain typed. *)
val try_create
  :  Configuration.t
  -> sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> default_model:string
  -> (Inference_host.t, Error.t) Result.t

val try_create_default
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> default_model:string
  -> (Inference_host.t, Error.t) Result.t

(** Ordinary CLI convenience selects Existing authority and OPENAI_API_KEY at
    this explicit entry boundary. Missing setup raises an actionable finite error;
    it never recreates authority or reenrolls a disabled environment binding. *)
val create
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> default_model:string
  -> Inference_host.t

val context : Inference_host.t -> Chat_response.Config.t -> Inference_runtime.Context.t

(** Standalone execution intentionally has no durable session ledger. Its fresh
    namespace and actual attempts remain independent of presentation observers. *)
val try_execution
  :  Inference_host.t
  -> Chat_response.Config.t
  -> (Inference_client.Execution.t, Inference_runtime.Preparation_error.t) Result.t

val execution : Inference_host.t -> Chat_response.Config.t -> Inference_client.Execution.t

(** Tighten raw response/frame bounds without changing the chosen host identity,
    account, endpoint or credentials. Existing captured targets can be resolved
    through this host without recapture. *)
val bounded_host : Inference_host.t -> max_body_bytes:int -> Inference_host.t

(** Explicit private auxiliary execution using the chosen local host and config.
    No remote session selection/credentials are inferred or forwarded. *)
val bounded_execution
  :  Inference_host.t
  -> max_body_bytes:int
  -> Chat_response.Config.t
  -> Inference_client.Execution.t

(** Explicit daemon composition using the same captured host. Historical
    targetless data has no implicit migration policy. These provisional runtime
    ports allocate fresh actual identities but intentionally do not claim a
    durable inference ledger; the ledger integration replaces their observers.
    Other daemon authority, quotas and transport limits retain their defaults. *)
val daemon_options : Inference_host.t -> Agent_server.Daemon.options

(** Installed provider operator remains reachable before explicit host setup.
    Factory binds the same dynamic inference backend to actual daemon identity. *)
val daemon_options_default
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> default_model:string
  -> Agent_server.Daemon.options

(** Explicit process host policy for newly prepared attempts, including restored
    targets. Existing prepared attempts and uncertainty remain immutable. *)
val try_create_default_with_policy
  :  transport_policy:Inference.Observation.Transport_policy.t
  -> sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> default_model:string
  -> (Inference_host.t, Error.t) Result.t

val create_with_policy
  :  transport_policy:Inference.Observation.Transport_policy.t
  -> sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> default_model:string
  -> Inference_host.t

val daemon_options_default_with_policy
  :  transport_policy:Inference.Observation.Transport_policy.t
  -> sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> default_model:string
  -> Agent_server.Daemon.options
