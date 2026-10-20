open! Core

(** Invocation-local compiler scheduling. A checkpoint checks cancellation and
    elapsed policy on every call, but enters the scheduler only on the first call
    and at most [max_checkpoints_between_yields] calls apart thereafter. [check]
    also runs immediately after a successful yield, before compiler work resumes.

    This mutable owner may transfer sequentially from metadata preparation to its
    joined compiler domain. It must never be invoked concurrently or recursively.
    Callback exceptions propagate unchanged without advancing the cadence. *)
type t

val max_checkpoints_between_yields : int
val create : check:(unit -> unit) -> yield:(unit -> unit) -> t
val run : t -> unit
