(** Pure ASCII-insensitive literal matching over an admitted readable projection.
    Search_entry owns coverage/size/UTF-8 admission; this module owns matching and
    bounded snippet construction. No authority or filesystem effects. *)
type t

val create : Agent_protocol.Search_term.t -> t

module Match : sig
  type t = private
    { part_index : int
    ; snippet : Agent_protocol.Search_snippet.t
    }
end

(** First occurrence in the first matching part; parts are not joined. Snippets
    contain at most 512 UTF-8 bytes and preserve the complete literal match.
    The service revalidates current authority/content before disclosing them. *)
val find : t -> Search_entry.t -> (Match.t option, Agent_protocol.Error.t) result
