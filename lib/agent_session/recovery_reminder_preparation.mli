open! Core

(** Final prepared-policy evidence. The live qualified binding and stable
    declaration revision serve different purposes. *)
type t

val of_selection : Session_context_capacity.Selection.t -> t option
val revision : t -> Chatml_source.Digest.t

(** Only measured or qualified estimated baselines in the warning band are
    eligible. An automatic selection leaves checkpoint-due input to rollover. *)
val warns : t -> Inference_runtime.Prepared.t -> bool

(** Check the exact alternative after including the advisory. *)
val fits : t -> Inference_runtime.Prepared.t -> bool

(** Fixed User-priority guidance, preserving existing guidance and all required
    input. No history entry or checkpoint authority is created. *)
val with_reminder : Inference.Request.t -> Inference.Request.t option
