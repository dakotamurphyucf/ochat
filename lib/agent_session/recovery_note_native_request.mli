open! Core

(** The selected tool name fixes the effect. Decoders validate finite JSON and
    typed revisions, paths, ranges, page bounds and content before any access. *)
type t =
  | Query of Recovery_note_query.t
  | Mutate of Recovery_notes.Catalog.mutation

val decode
  :  effect_kind:Recovery_note_access.Effect.t
  -> Jsonaf.t
  -> (t, Agent_protocol.Error.t) result

val decode_text_exn : string -> Jsonaf.t
val parameters : Recovery_note_access.Effect.t -> Jsonaf.t
