(** Hard protocol safety ceilings, independent of lower actor/runtime quotas.
    4096 occurrences matches the existing bounded retained-work observation
    ceiling; records/results cannot retain an unbounded second work graph.
    Document bytes count encoded JSON UTF-8 bytes; depth counts JSON containers. *)
val max_occurrences : int

val max_document_bytes : int
val max_depth : int
val check_count : int -> (unit, Error.t) result
val list : (Jsonaf.t -> ('a, Error.t) result) -> Jsonaf.t -> ('a list, Error.t) result
