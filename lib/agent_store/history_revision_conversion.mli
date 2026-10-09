(** Structural revision initialization used only by named legacy document
    conversion steps. These functions visit owned history envelopes, preserve
    unknown fields, and leave existing revisions for strict current validation.
    They do not recursively interpret opaque payloads or act as runtime readers. *)
open! Core

val initialize_history_revision : Jsonaf.t -> (Jsonaf.t, Document_schema.Error.t) Result.t

val initialize_history_revisions
  :  Jsonaf.t
  -> (Jsonaf.t, Document_schema.Error.t) Result.t

val initialize_history_window : Jsonaf.t -> (Jsonaf.t, Document_schema.Error.t) Result.t
val initialize_snapshot_history : Jsonaf.t -> (Jsonaf.t, Document_schema.Error.t) Result.t
val initialize_event_history : Jsonaf.t -> (Jsonaf.t, Document_schema.Error.t) Result.t

val initialize_method_result
  :  Jsonaf.t
  -> method_name:string
  -> (Jsonaf.t, Document_schema.Error.t) Result.t
