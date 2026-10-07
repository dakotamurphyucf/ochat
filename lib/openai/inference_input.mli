open! Core

(** Private pure adapter lowering. Canonical authored input is encoded directly;
    reconstructed legacy input is explicitly validated as such. Actual captures
    require exact provenance and matching independently decoded semantics before
    raw replay. No provider IDs replace host identities. *)
val prepare
  :  Responses_driver.Profile.t
  -> Inference.Request.t
  -> (Responses_driver.Prepared.t, Inference_runtime.Preparation_error.t) Result.t

val origin
  :  Inference.Request.Target.t
  -> (History_entry.Payload.Origin.t, Inference_runtime.Preparation_error.t) Result.t
