(** Revision and durable-event position returned by accepted session mutations. *)

type t =
  { revision : int64
  ; latest_event_sequence : int64
  }
[@@deriving sexp]

(** [to_fields t] encodes fields for embedding in a result object. *)
val to_fields : t -> (string * Jsonaf.t) list

(** [of_fields fields] decodes nonnegative revision and event sequence fields. *)
val of_fields : Json_codec.fields -> (t, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
