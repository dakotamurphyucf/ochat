(** Bounded admission shared by recovery coordinates and membership manifests. *)
val limits : max_bytes:int -> max_fields:int -> max_nodes:int -> Document_schema.Limits.t

val measure
  :  limits:Document_schema.Limits.t
  -> Jsonaf.t
  -> (int, Protocol_error.t) result

val admit_coordinate : Jsonaf.t -> (unit, Protocol_error.t) result
val fields : Jsonaf.t -> string list -> (Json_codec.fields, Protocol_error.t) result

(** Canonical, nonnegative decimal strings for immutable recovery coordinates. *)
val revision_to_json : int64 -> Jsonaf.t
val revision_of_json : Jsonaf.t -> (int64, Protocol_error.t) result
