open Core

(** Incremental SSE framing. Feed lines without the newline terminator (a trailing
    CR is accepted). A blank line dispatches a frame; EOF discards an unfinished
    frame as required by SSE. Semantic completion is checked by the wire tracker,
    never inferred from EOF or [[DONE]]. No JSON payload is silently discarded. *)
type t

type frame =
  | Payload of Jsonaf.t
  | Done

val create : ?max_frame_bytes:int -> unit -> t Or_error.t

(** Single-owner mutable parser. Errors poison it; callers must stop reading.
    The size bound includes ignored fields/comments, not just data. *)
val feed_line : t -> string -> frame option Or_error.t

(** Reports whether undispatched data remained. Does not dispatch it. *)
val finish : t -> bool
