open! Core

(** Pure legacy ChatMD authoring ingress. Config owns exactly these provider
    setting names; omitted child fields inherit. This module neither selects a
    profile nor supplies a default model, credentials or runtime context. *)
val setting_names : string list

(** A finite JSON number, including integral floats. Malformed/nonfinite values
    and configured admission bounds reject before preparing an inference. *)
val number_of_float
  :  float
  -> limits:Document_schema.Limits.t
  -> (Jsonaf.t, Inference_runtime.Preparation_error.t) Result.t

(** Capture supplied fields only. The existing reasoning_effort shorthand expands
    to effort plus detailed summary. show_tool_call and id remain host UI/session
    settings and are never sent to inference. *)
val settings
  :  Config.t
  -> limits:Document_schema.Limits.t
  -> (Inference.Request.Setting.t list, Inference_runtime.Preparation_error.t) Result.t

val apply_overrides
  :  Inference.Request.Target.t
  -> Config.t
  -> limits:Document_schema.Limits.t
  -> (Inference.Request.Target.t, Inference_runtime.Preparation_error.t) Result.t
