open Core

(** Private anchor frames:16 bytes, fixed magic/version, finite tag, big-endian
    uint32 status and zero reserved bytes. Decoding validates the complete frame;
    native signal numbers are normalized only in the parent. *)
type t =
  | Ready
  | Exited of int
  | Signaled of int
  | Failed
[@@deriving equal, sexp_of]

val frame_bytes : int
val decode : string -> t option
