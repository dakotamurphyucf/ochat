open Core

(** Internal qualified-host authoring setup. Readonly helper defaults do not
    grant shell, file, network or child-session execution authority. *)
type t

(** Supply installed targets/catalog for an extensibility host, retaining an
    explicitly configured host and its source/compiler limits. *)
val configure_host
  :  ?host:Chat_response.Authoring_validation.host
  -> policy:Chat_response.One_off_request.policy
  -> unit
  -> (Chat_response.Authoring_validation.host, string) result

(** Add helper declarations only when selected implementations or explicit
    authored help request automatic guidance. Manual retains exact declarations.
    This runs before construction; final admission still validates the actual
    native/managed metadata and requires authentic helper bindings. *)
val augment
  :  registrations:Chat_response.Agent_runtime.native_registration list
  -> Prompt.Chat_markdown.top_level_elements list
  -> (Prompt.Chat_markdown.top_level_elements list, string) result

(** Resolve source policy against the fully constructed registry. An admitted
    inherited policy may be supplied instead. Validate complete preload assembly
    before tools can be exposed. [elements] is the original source, before helper
    augmentation, so the policy retains which helpers it added. No factory is
    needed for manual/ordinary tools. *)
val prepare
  :  ?admitted:Chat_response.Authoring_policy.t
  -> host:Chat_response.Authoring_validation.host
  -> elements:Prompt.Chat_markdown.top_level_elements list
  -> capabilities:Chat_response.Tool_capability.t
  -> unit
  -> (t option, string) result

val materialize
  :  t
  -> input:Operation_worker.Input.t
  -> (Chat_response.Authoring_materialization.t, Agent_protocol.Error.t) result
