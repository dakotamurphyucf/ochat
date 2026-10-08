open! Core
module Driver = Openai.Responses_driver

(** Pure host profile policy. Protocol feature declarations do not authorize
    credentials or assert every account can access a selected model. *)
type route =
  | Public_api
  | Direct_codex
[@@deriving equal, sexp_of]

module Transport_policy : sig
  (** Exact values: sse, prefer-websocket, require-websocket. No aliases,
      environment/probe/default lookup, or transport fallback during parsing. *)
  val of_string : string -> Inference.Observation.Transport_policy.t Or_error.t

  val to_string : Inference.Observation.Transport_policy.t -> string
end

(** Canonical selected first-party Responses endpoint. Not an inferred URL rule. *)
val endpoint : route -> string

(** Shared current SSE feature/setting defaults, including explicit Unknown WS.
    Qualification harness may reuse these while constructing its separately
    trusted, provisional exact-model trial declarations. *)
val baseline : route -> (Driver.Capability.feature * Driver.Capability.support) list

(** Adds only committed qualified exact-model protocol declarations for this
    route at its exact first-party endpoint. Unlisted models and every endpoint
    override remain Unknown for WS. There are no account IDs, credential probes,
    model-prefix rules, candidate declarations or arbitrary JSON in the catalog.
    Credential/account admission remains the registry/bridge's independent job.
    Initially no Supported WS model rows exist; live qualification publishes them
    with dated evidence in versioned documentation. *)
val capabilities : route -> endpoint:string -> Driver.Capability.t Or_error.t
