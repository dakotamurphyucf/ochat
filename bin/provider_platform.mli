open! Core
module P = Agent_protocol
module DTO = P.Provider_operator

(** Trusted local defaults and qualified direct-Codex ports. No startup key probe
    or login. Runtime host authority is opened once by its daemon factory with
    actual server identity; the inference backend points to that same owner. *)
type t

(** Fresh nonsecret namespace using the explicit host randomness port. *)
val new_namespace : Eio_unix.Stdenv.base -> string

val create
  :  ?compatible_profiles:Inference_host.Compatible_profile.t list
  -> ?transport_policy:Inference.Observation.Transport_policy.t
  -> sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> home:string
  -> api_url:string option
  -> lookup:(string -> string option)
  -> default_model:string
  -> namespace:string
  -> callback_port:int
  -> unit
  -> (t, DTO.Error.t) Result.t

val host : t -> Inference_host.t
val factory : t -> Agent_server.Provider_operator_port.factory

(** Standalone CLI uses an explicit validated host identity and this same
    composition; it does not initialize credentials by opening the service. *)
val open_operator
  :  t
  -> server_id:P.Id.Server.t
  -> (Provider_runtime.t, DTO.Error.t) Result.t
