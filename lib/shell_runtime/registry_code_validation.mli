open Core

(** Whole-invocation native-footprint validation before fresh hook construction.
    Keys are compared with the same bounded budget, using a temporary comparator
    that never escapes. The result preserves original native runtime order and
    original tool map keys; it contains no callback or new native capability. *)
type t

val runtimes : t -> (Runtime.t * Chatmd_shell_spec.Shell_spec.t) list
val tools : t -> Chatmd_shell_spec.Shell_tool_spec.t String.Map.t

val create
  :  original_manifest:Chatmd_shell_spec.Manifest.t
  -> original_runtimes:Runtime.t list
  -> original_tools:Chatmd_shell_spec.Shell_tool_spec.t String.Map.t
  -> original_policy:Admin_policy.t
  -> candidate_manifest:Chatmd_shell_spec.Manifest.t
  -> candidate_runtimes:Chatmd_shell_spec.Shell_spec.t list
  -> candidate_policy:Admin_policy.t
  -> budget:Registry_code_budget.t
  -> (t, Registry_resource_footprint.Error.t) Result.t
