(** A disclosed occurrence in current canonical history; identity is host-owned.
    Revisions are observations, not authorization or proof the hit still exists.
    The snippet contains only readable user/assistant text. Part index is the
    zero-based public message-content position, including non-text parts. *)
type t [@@deriving sexp_of]

val create
  :  session:Session_ref.t
  -> generation:int
  -> session_revision:int64
  -> history_id:History.Id.t
  -> content_revision:History.Content_revision.t
  -> part_index:int
  -> snippet:Search_snippet.t
  -> (t, Error.t) result

val session : t -> Session_ref.t
val generation : t -> int
val session_revision : t -> int64
val history_id : t -> History.Id.t
val content_revision : t -> History.Content_revision.t
val part_index : t -> int
val snippet : t -> Search_snippet.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
