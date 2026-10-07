open! Core

(** [collapse_read_file_history ?placeholder items] replaces stale read-file
    output payloads without changing list order or cardinality. *)
val collapse_read_file_history
  :  ?placeholder:string
  -> Openai.Responses.Item.t list
  -> Openai.Responses.Item.t list

(** [collapse_read_file_entries ?placeholder entries] prepares request
    entries by replacing stale read-file payloads while preserving every
    application-owned ID. Bound host call occurrences take precedence; unresolved
    legacy outputs use the nearest preceding matching call alias. Untouched
    entries retain their complete payload (including captures/future members);
    changed results become Authored because opaque captures no longer apply. *)
val collapse_read_file_entries
  :  ?placeholder:string
  -> History_entry.t list
  -> History_entry.t list
