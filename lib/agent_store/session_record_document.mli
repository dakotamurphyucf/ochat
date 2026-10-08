(** Named storage projection shared by session metadata and index owners.
    Nested tagged shapes retain unknown fields. Counter strings are validated
    before adapting to the current protocol decoder; optional field presence
    follows the concrete Session contract, including inference-summary null. *)
open! Core

type t = Agent_protocol.Session.t

val shape : Document_schema.Shape.t
val of_json : Jsonaf.t -> (t, Document_schema.Error.t) Result.t
val to_json : t -> (Jsonaf.t, Document_schema.Error.t) Result.t

(** Structural original-version identity projection, before domain conversion. *)
val stored_id
  :  Jsonaf.t
  -> (Agent_protocol.Id.Session.t, Document_schema.Error.t) Result.t
