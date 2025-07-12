(** Minimal blocking path completion helper – **temporary implementation**.
    It will be replaced by an asynchronous, cache-backed version in a future
    refactor once command-mode stabilises. *)

type t

val create : unit -> t

(** [suggestions t ~fs ~cwd ~prefix] returns a (possibly empty) sorted list of
    entries starting with [prefix].  Directory names get a trailing [/].  The
    function is cheap after the first call thanks to a per-directory cache
    with TTL-based invalidation. *)
val suggestions : t -> fs:'a Eio.Path.t -> cwd:string -> prefix:string -> string list

(** [next t ~dir] cycles through the most recent {!suggestions} result.
    Returns [None] if the cache is empty. *)
val next : t -> dir:[ `Fwd | `Back ] -> string option

val reset : t -> unit
