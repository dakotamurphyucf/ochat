open! Core

(** Runtime-host storage composition, separate from session roots and daemon
    access credentials. Configuration contains capabilities and nonsecret labels,
    not provider key bytes. No ambient path/environment discovery occurs here. *)
module Mode : sig
  type t =
    | Existing
    | Initialize of Credential_registry_model.Id.t
end

module Error : sig
  type t =
    | Invalid_configuration
    | Storage of Private_storage.Error.t
    | Secret_store of Provider_secret_store.Error.t
    | Lifecycle of Credential_registry.Error.t
    | Bridge of Credential_bridge.Error.t
  [@@deriving sexp_of]
end

type t

(** Existing never creates/replaces authority when metadata is missing, corrupt or
    unsupported. Initialize is explicit create-only provisioning with its supplied
    incarnation. Neither mode enrolls or reenables a binding automatically. *)
val create
  :  anchor:Eio.Fs.dir_ty Eio.Path.t
  -> components:Private_storage.Name.t list
  -> host:Credential_registry_model.Id.t
  -> secret_namespace:Provider_secret_store.Namespace.t
  -> mode:Mode.t
  -> (t, Error.t) Result.t

module Opened : sig
  type t

  val bridge : t -> Credential_bridge.t

  (** Trusted host administration only; never exposed to session code or clients. *)
  val registry : t -> Credential_registry.t
end

(** The caller switch owns the directory, backend and registry through all graph
    and attempt lifetimes. Shutdown first cancels/drains those workers, then closes
    the registry, backend and directory. Failed acquisition closes owned resources.
    Explicit environment and OAuth ports belong to the runtime host. *)
val open_host
  :  t
  -> sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> driver:Openai.Responses_driver.t
  -> new_operation:(unit -> Credential_registry_model.Id.t)
  -> environment:Credential_bridge.Environment.t option
  -> oauth:Credential_bridge.OAuth.t option
  -> mappings:Credential_bridge.Mapping.t list
  -> authorize:
       (principal:string
        -> profile:string
        -> operation:Credential_bridge.Operation.t
        -> bool)
  -> maximum_wait:Time_ns.Span.t
  -> transport_policy:Inference.Observation.Transport_policy.t
  -> limits:Inference_runtime.Limits.t
  -> (Opened.t, Error.t) Result.t
