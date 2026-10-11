open Core

(** Complete ordered source-only results and one cumulative attempt receipt. *)
type t

val create
  :  Project_prepare_source_result.t list
  -> accounting:Project_prepare_accounting.t
  -> t

val preparations : t -> Project_prepare_source_result.t list
val accounting : t -> Project_prepare_accounting.t
