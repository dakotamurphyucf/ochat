(** Explicit storage ownership for embedded protocol records. Raw payloads are retained by their domain values. *)
val error : Document_schema.Shape.t

val history_entry : Document_schema.Shape.t
val lifecycle : Document_schema.Shape.t
val protocol_spec : Document_schema.Shape.t
val operation : Document_schema.Shape.t
val permission : Document_schema.Shape.t
val grant : Document_schema.Shape.t
val invocation : Document_schema.Shape.t
val job : Document_schema.Shape.t
val schedule : Document_schema.Shape.t
val moderator_execution : Document_schema.Shape.t
val subscription : Document_schema.Shape.t
val delivery : Document_schema.Shape.t
val attachment : Document_schema.Shape.t
val observed : Document_schema.Shape.t
val history_window : Document_schema.Shape.t
val event_session : Document_schema.Shape.t
val public_schedule : Document_schema.Shape.t
val workspace : Document_schema.Shape.t
