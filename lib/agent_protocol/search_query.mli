(** Conversation search reuses catalog selectors. Page limit counts hits (1–100),
    while scan_limit bounds examined canonical entries (1–512), including misses.
    Ordering is Created_at/Ascending with the catalog's stable session-ID ties,
    then canonical entry order. A zero-hit partial page can have a continuation.
    Query JSON is bounded to 64 KiB with at most 128 label filters.
    Cursors belong to search and cannot be reused for catalog listing. *)
type t [@@deriving sexp]

val create
  :  server_id:Id.Server.t
  -> term:Search_term.t
  -> catalog:Session.List_request.t
  -> scan_limit:int
  -> (t, Error.t) result

val server_id : t -> Id.Server.t
val term : t -> Search_term.t
val catalog : t -> Session.List_request.t
val scan_limit : t -> int
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
