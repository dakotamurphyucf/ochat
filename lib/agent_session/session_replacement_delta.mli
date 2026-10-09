(** Private evidence for the supported administrative replacement shapes:
    [Created] or exactly [Batch [Pending_inputs_changed; Created]]. No nested
    last-replacement inference. The ordinary delta reducer validates the prefix
    and replacement; this classifier grants no admission or pruning authority. *)
type t

val classify : Session_delta.t -> t option
val state : t -> Session_state.t

(** Replaces the terminal state while retaining the exact ordered pending prefix. *)
val with_state : t -> Session_state.t -> Session_delta.t
