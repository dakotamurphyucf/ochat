open! Core

(** OpenAI Responses implementation of the neutral inference boundary. Capture
    selection before runtime activation; prepare performs no authentication or
    network I/O. A host supplies the profile, actual optional revision, driver,
    and exact credential resolver. No ambient model/key/profile lookup. *)
val capture_target
  :  Responses_driver.Profile.t
  -> profile_revision:string option
  -> model:string
  -> settings:Responses_driver.Setting.t list
  -> limits:Document_schema.Limits.t
  -> (Inference.Request.Target.t, Inference_runtime.Preparation_error.t) Result.t

(** Legacy descriptor producer ingress only. Hosted tools reject. No permission
    or execution binding is implied by a schema. *)
val tool_spec
  :  Responses.Request.Tool.t
  -> limits:Document_schema.Limits.t
  -> (Inference.Request.Tool_spec.t, Inference_runtime.Preparation_error.t) Result.t

(** Fixed legacy composition accepts only absent auth binding by default. Dynamic
    hosts supply the exact admitted binding. Revision is capture provenance and
    must be supplied from the capture only after host compatibility admission. *)
val create
  :  ?auth_binding:Inference.Request.Auth_binding.t History_entry.Payload.Presence.t
  -> Responses_driver.t
  -> profile:Responses_driver.Profile.t
  -> profile_revision:string option
  -> auth:Responses_driver.Auth.resolver
  -> limits:Inference_runtime.Limits.t
  -> (Inference_runtime.Adapter.t, Inference_runtime.Preparation_error.t) Result.t
