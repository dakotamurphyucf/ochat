module Provider_profiles = Provider_profiles
open! Core

(** Explicit application composition for the initial OpenAI backend. Lower-level
    routes receive Context and Identity; they never call this module to recover
    missing configuration. No environment lookup, credential forwarding, login,
    model catalog or migration policy is supplied here. *)
type t

val create
  :  Openai.Responses_driver.t
  -> profile:Openai.Responses_driver.Profile.t
  -> profile_revision:string option
  -> auth:Openai.Responses_driver.Auth.resolver
  -> default_model:string
  -> namespace:string
  -> limits:Inference_runtime.Limits.t
  -> (t, Inference_runtime.Preparation_error.t) Result.t

(** Host-owned bounded auxiliary route. Preserves profile, credential resolver,
    default model and identity allocator exactly; only tightens raw response/frame
    byte limits. Resolve the already captured target against the returned host.
    No recapture, alternate account, model selection or network/auth effect. *)
val with_response_limit
  :  t
  -> max_body_bytes:int
  -> (t, Inference_runtime.Preparation_error.t) Result.t

(** Capture a NEW root prompt using the host's explicit default model when its
    authoring model is absent. Legacy reasoning_effort expands to the existing
    effort+detailed-summary convention. Profile setting defaults are captured
    exactly once. No I/O, runtime activation or durable admission occurs. *)
val capture_config
  :  t
  -> Chat_response.Config.t
  -> (Inference.Request.Target.t, Inference_runtime.Preparation_error.t) Result.t

(** An explicit root prompt revision. Preserve current unknown target/setting
    members, replace the root model and legacy config-owned settings from the
    new effective capture, and leave settings not owned by legacy Config alone.
    Host authorization and atomic source/target persistence remain mandatory. *)
val recapture_config
  :  t
  -> current:Inference.Request.Target.t
  -> Chat_response.Config.t
  -> (Inference.Request.Target.t, Inference_runtime.Preparation_error.t) Result.t

(** Child config omission INHERITS. Applies only supplied model/max_tokens/
    temperature/reasoning_effort edits through preserving Target operations;
    existing explicit parent account/profile/endpoint always remain selected. *)
val override_config
  :  Inference.Request.Target.t
  -> Chat_response.Config.t
  -> (Inference.Request.Target.t, Inference_runtime.Preparation_error.t) Result.t

val resolve : t -> Inference_runtime.resolver

(** One thread-safe allocator per explicit fresh namespace. Allocation always
    occurs regardless of UI observers; nested scopes retain the actual relation.
    The host must never reuse this namespace after restart. Durable daemon hosts
    instead supply their persisted ledger allocator at the existing Identity port.
    Counter exhaustion raises an invariant failure rather than wrapping/reusing
    an ID. This allocator neither persists an attempt nor grants dispatch. *)
val identity : t -> Chat_response.Neutral_turn.Identity.t
