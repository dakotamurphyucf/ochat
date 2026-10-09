(** Bounded traversal progress, not a relevance-ranked or exact-count result.
    A partial page must carry a continuation and make observable scan progress;
    it may contain zero hits. Only a complete traversal sets reached_end. *)
type t [@@deriving sexp]

val create
  :  hits:Search_hit.t list
  -> next_cursor:Page.Cursor.t option
  -> reached_end:bool
  -> scanned_entries:int
  -> scanned_sessions:int
  -> (t, Error.t) result

val hits : t -> Search_hit.t list
val next_cursor : t -> Page.Cursor.t option
val reached_end : t -> bool
val scanned_entries : t -> int
val scanned_sessions : t -> int
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
