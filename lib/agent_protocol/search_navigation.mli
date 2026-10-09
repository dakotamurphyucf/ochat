(** Navigation is a fresh authorized lookup, never authority conveyed by a hit.
    The supplied snippet is ignored. The query reapplies current catalog filters;
    its pagination cursor is not part of navigation. *)
module Request : sig
  type t

  val create : query:Search_query.t -> hit:Search_hit.t -> (t, Error.t) result
  val query : t -> Search_query.t
  val hit : t -> Search_hit.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
  val sexp_of_t : t -> Sexplib.Sexp.t
  val t_of_sexp : Sexplib.Sexp.t -> t
end

(** Plain text from one readable canonical entry. Text is at most 2048 UTF-8
    bytes; [truncated] explicitly records omitted text. No raw provider data,
    reasoning, image URI or tool payload is represented. *)
module Entry : sig
  type t

  val create
    :  history_id:History.Id.t
    -> content_revision:History.Content_revision.t
    -> text:string
    -> truncated:bool
    -> (t, Error.t) result

  val history_id : t -> History.Id.t
  val content_revision : t -> History.Content_revision.t
  val text : t -> string
  val truncated : t -> bool
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Response : sig
  type t = private
    | Current of
        { hit : Search_hit.t
        ; context : Entry.t list
        }
    | Changed of History.Content_revision.t
    | Unavailable

  (** Up to five chronological canonical positions around the target. Excluded
      positions are omitted, not replaced by more distant entries. The context
      includes the target and has unique IDs; its revision agrees with [hit]. *)
  val current : hit:Search_hit.t -> context:Entry.t list -> (t, Error.t) result

  val changed : History.Content_revision.t -> t
  val unavailable : t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
  val sexp_of_t : t -> Sexplib.Sexp.t
  val t_of_sexp : Sexplib.Sexp.t -> t
end
