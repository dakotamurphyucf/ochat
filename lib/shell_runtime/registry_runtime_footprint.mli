open Core

(** Exact effective runtime declarations except diagnostic Source_ref. Returns
    the first differing native field, preserving the shared invocation budget. *)
val check
  :  Chatmd_shell_spec.Shell_spec.t
  -> candidate:Chatmd_shell_spec.Shell_spec.t
  -> budget:Registry_code_budget.t
  -> (unit, string) Result.t
