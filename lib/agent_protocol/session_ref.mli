(** Persistent host-qualified identity; carries no connection or execution grant. *)
type t [@@deriving compare, equal, sexp_of]

val create : server_id:Id.Server.t -> session_id:Id.Session.t -> t
val server_id : t -> Id.Server.t
val session_id : t -> Id.Session.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
