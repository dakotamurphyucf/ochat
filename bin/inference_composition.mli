open! Core

(** Pure conversion of the existing CLI API_URL convention to a Responses
    endpoint. Profile construction performs endpoint validation. *)
val responses_endpoint : api_url:string option -> string

(** Explicit standard OpenAI composition for first-party command entry points.
    Captures the host key and API_URL once. API_URL is a host or absolute
    base URL; HTTPS is the default, and the Responses path is appended. Invalid
    endpoints fail profile validation without falling back to another host. The caller chooses the default model and supplies
    an environment whose cryptographic RNG has already been initialized. The
    profile permits the adapter's implemented fields; it does not assert that
    every model accepts each field. Provider rejection remains a typed failure. *)
val create : env:Eio_unix.Stdenv.base -> default_model:string -> Inference_host.t

val context : Inference_host.t -> Chat_response.Config.t -> Inference_runtime.Context.t

(** Standalone execution intentionally has no durable session ledger. Its fresh
    namespace and actual attempts remain independent of presentation observers. *)
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
