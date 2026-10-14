open Core

(** Static compiler output only. Both forms execute under the caller's existing
    budget and actual runtime installer; neither grants tools or loads sources. *)
type t =
  | Single of Chatml_host_runtime.compiled_script
  | Prepared of Chatml_linked_program.t

(** Source revision for saved state and deferred work. Preserve the caller's
    existing single-file identity; linked code uses the complete prepared graph
    fingerprint, including dependencies, scopes and its compiler contract. This
    is distinct from the entry body's content digest retained for custody. *)
val source_revision : t -> single_file_revision:string -> string

(** Dispatch without introducing another execution scope, preserving result
    validation/commit and cancellation behavior of the existing adapters. *)
val run_in_scope
  :  t
  -> ?scoped_async_operation:Chatml_host_runtime.scoped_async_operation
  -> ?prepare_result:
       (value:Chatml.Chatml_lang.value
        -> local_effects:Chatml.Chatml_lang.eff list
        -> (unit -> unit, string) result)
  -> control:Chatml.Chatml_lang.execution_control option
  -> config:Chatml_host_runtime.runtime_config
  -> entrypoint:string
  -> arguments:Chatml.Chatml_lang.value list
  -> unit
  -> (Chatml.Chatml_lang.value, Chatml_execution.error) result
