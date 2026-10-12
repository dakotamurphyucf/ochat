open Core

(** Typed primitives shared by the runtime and tool/administrative footprint
    walks. All input-sized strings poll the same invocation budget in bounded
    chunks; collection nodes are charged before inspecting retained fields. *)
val scalar : ('a -> 'a -> bool) -> Registry_code_budget.t -> 'a -> 'a -> bool

val string : Registry_code_budget.t -> string -> string -> bool
val bool : Registry_code_budget.t -> bool -> bool -> bool
val int : Registry_code_budget.t -> int -> int -> bool
val int64 : Registry_code_budget.t -> int64 -> int64 -> bool
val float : Registry_code_budget.t -> float -> float -> bool

val duration
  :  Registry_code_budget.t
  -> Chatmd_shell_spec.Duration.t
  -> Chatmd_shell_spec.Duration.t
  -> bool

val bytes
  :  Registry_code_budget.t
  -> Chatmd_shell_spec.Duration.bytes
  -> Chatmd_shell_spec.Duration.bytes
  -> bool

val option
  :  (Registry_code_budget.t -> 'a -> 'a -> bool)
  -> Registry_code_budget.t
  -> 'a option
  -> 'a option
  -> bool

val list
  :  (Registry_code_budget.t -> 'a -> 'a -> bool)
  -> Registry_code_budget.t
  -> 'a list
  -> 'a list
  -> bool

val setting
  :  (Registry_code_budget.t -> 'a -> 'a -> bool)
  -> Registry_code_budget.t
  -> 'a Chatmd_shell_spec.Shell_spec.setting
  -> 'a Chatmd_shell_spec.Shell_spec.setting
  -> bool

val path
  :  Registry_code_budget.t
  -> Chatmd_shell_spec.Path_expr.t
  -> Chatmd_shell_spec.Path_expr.t
  -> bool
