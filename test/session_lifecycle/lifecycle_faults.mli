(** One-shot filesystem failures in the real service's owned store. *)
type phase =
  | Authority_acknowledgement
  | Payload_deletion
  | Final_cleanup
  | Rejection_completion

type t

val create : unit -> t
val arm : t -> phase -> unit
val arm_cancel : t -> phase -> cancel:(unit -> unit) -> unit
val was_triggered : t -> bool
val wrap_env : t -> Eio_unix.Stdenv.base -> Eio_unix.Stdenv.base
