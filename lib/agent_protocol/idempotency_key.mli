(** Client-generated keys used to make mutating commands safely repeatable. *)

type t [@@deriving compare, equal, hash, sexp]

(** [of_string encoded] validates a nonempty, bounded wire key. *)
val of_string : string -> (t, Error.t) result

(** [to_string t] returns the validated wire representation. *)
val to_string : t -> string

(** [of_json json] decodes a JSON string key. *)
val of_json : Jsonaf.t -> (t, Error.t) result

(** [to_json t] encodes a JSON string key. *)
val to_json : t -> Jsonaf.t
