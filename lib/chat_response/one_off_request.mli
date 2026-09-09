open Core

(** Host ceilings for the agent-facing one-off tool. Unlike the generic language
    evaluator this interface always has bounded policy. Requests may only lower
    these values; they cannot opt into unrestricted execution. *)
type policy =
  { compilation : Chatml_compilation.limits
  ; execution : Chatml_execution.limits
  ; max_output_bytes : int
  }

val default_policy : policy

type t = private
  { source : string
  ; input : Jsonaf.t
  ; tools : string list
  ; policy : policy
  }

(** The stable request shape. Optional [timeout_ms] and [limits] are validated
    against the host policy by [decode], not by model-provided metadata. *)
val parameters : Jsonaf.t

(** Strict non-executing validation, including duplicate/unknown fields, bounded
    input, explicit tool selection and non-widening limit overrides. Does not
    compile, evaluate, load source files or resolve/grant tool capabilities. *)
val decode : policy:policy -> Jsonaf.t -> (t, Chatmd_shell_spec.Diagnostic.t list) result

(** Adapt effective policy to the existing owned script service. *)
val script_limits : t -> Chatmd_shell_spec.Chatmd_script_spec.limits
