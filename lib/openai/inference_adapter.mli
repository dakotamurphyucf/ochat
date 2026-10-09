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

module Auth_source : sig
  type t =
    | Static of Responses_driver.Auth.resolver
    | Capture of
        (target:Inference.Request.Target.t
         -> ( Responses_driver.Auth.resolver
              , Inference_runtime.Preparation_error.t )
              Result.t)

  (** Capture is trusted non-yielding policy preflight, called once per plan after
      pure request/configuration validation. It must not retrieve credentials,
      perform network I/O or initiate login. Each plan retains its returned
      resolver independently. An error rejects preparation without fallback.
      Dispatch still freshly authorizes and obtains a guarded attempt lease. *)
end

(** Fixed legacy composition accepts only absent auth binding by default. Dynamic
    hosts supply the exact admitted binding. Revision is capture provenance and
    must be supplied from the capture only after host compatibility admission. *)
val create
  :  ?auth_binding:Inference.Request.Auth_binding.t History_entry.Payload.Presence.t
  -> ?check_current:(unit -> (unit, Inference_runtime.Preparation_error.t) Result.t)
  -> Responses_driver.t
  -> profile:Responses_driver.Profile.t
  -> profile_revision:string option
  -> auth:Auth_source.t
  -> limits:Inference_runtime.Limits.t
  -> (Inference_runtime.Adapter.t, Inference_runtime.Preparation_error.t) Result.t
