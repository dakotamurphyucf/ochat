(** Grapheme-safe byte offsets for valid UTF-8 editor buffers. *)

(** [floor text pos] clamps to the preceding extended-grapheme boundary. *)
val floor : string -> int -> int

(** [ceil text pos] clamps to the following extended-grapheme boundary. *)
val ceil : string -> int -> int

val previous : string -> int -> int

(** Adjacent extended-grapheme boundaries, clamped at the buffer ends. *)
val next : string -> int -> int

(** [uchar u] encodes one Unicode scalar as UTF-8. *)
val uchar : Stdlib.Uchar.t -> string
