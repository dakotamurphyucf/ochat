open Core
module Model = Project_build_model

(** Invocation-local cooperative work accounting. No checkpoint or mutable counter
    survives composition. Limit exceptions are private to [run]; exceptions from
    the supplied checkpoint propagate unchanged, even when nested calls occur. *)
type t

val run
  :  Model.Limits.t
  -> checkpoint:(unit -> unit)
  -> f:(t -> ('a, Model.Error.t) Result.t)
  -> ('a * Model.Accounting.t, Model.Error.t) Result.t

val checkpoint : t -> unit
val compare_string : t -> string -> string -> int
val compare_id : t -> Chatml_source.Id.t -> Chatml_source.Id.t -> int
val equal_source : t -> Chatml_source.t -> Chatml_source.t -> bool

(** Overflow-checked accounting for supplied exact source and metadata bytes.
    Uses the same max_input_bytes ceiling as subsequent Prepared admission; this
    early bound does not replace Prepared's complete metadata accounting. *)
val charge_bytes : t -> int -> unit
