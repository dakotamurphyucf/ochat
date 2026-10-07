(** Pure semantic presentation and grouping for compaction. No provider decode,
    opaque replay, capture mutation or canonical allocation. *)
val render : History_entry.t list -> string

val is_policy : History_entry.t -> bool
val is_reminder : History_entry.t -> bool
val is_shared : History_entry.t -> bool

(** Calls/results stay in contiguous groups. Bound host occurrences are
    authoritative; unresolved legacy results use the nearest preceding matching
    kind/alias. Unanswered calls retain the remaining suffix as one group. *)
val grouped : History_entry.t list -> History_entry.t list list

val message : role:History_entry.Payload.Role.t -> string -> History_entry.Payload.t
