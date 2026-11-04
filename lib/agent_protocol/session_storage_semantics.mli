(** Explicit feature names understood by stored session state/delta children.
    Store transaction and snapshot envelopes do not support these semantics on
    themselves. These constants convey no runtime or admission authority. *)
val recurrence_run_origin : string

val supported_children : string list
