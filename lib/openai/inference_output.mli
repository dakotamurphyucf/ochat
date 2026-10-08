open! Core

(** One attempt's private output projector; single serialized callback owner.
    Uses actual output positions as scoped item identities, independent of
    provider aliases. No canonical append, tool execution or Source_finished. *)
type t

val create
  :  target:Inference.Request.Target.t
  -> scope:Transcript.Scope.t
  -> accounting_id:Inference.Observation.Observation_id.t
  -> limits:Inference_runtime.Limits.t
  -> (t, Inference_runtime.Preparation_error.t) Result.t

(** Actual driver event projection. Only accepted updates publish; exact codec
    duplicates are ignored. Candidate eligibility comes from Wire.Item.local_call,
    including opaque caller validation. All raw item captures are retained. *)
val event : t -> Responses_driver.Event.t -> on_event:(Inference.Event.t -> unit) -> unit

(** Validated final response arrays retain actual order. Existing exact item.done
    candidates remain authoritative rather than being regenerated from terminal
    envelopes; terminal-only slots retain normal projection and eligibility.
    Failures without a response retain
    validated finalized prefix. Does not publish the final usage or terminal. *)
val finish
  :  t
  -> (Responses_driver.Terminal.t, Responses_driver.Auth.error) Result.t
  -> Inference_runtime.Receipt.t
