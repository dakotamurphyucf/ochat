(** Metadata-only, bounded SSE diagnostics for isolated provider fixtures.
    Preserve bytes and body length; record event names, not payload contents.
    This wrapper observes body consumption, not socket-level receipt. *)

type t

val create : unit -> t
val wrap : t -> Piaf.Response.t -> Piaf.Response.t
val metrics : t -> (string * Jsonaf.t) list
val self_check : unit -> unit
