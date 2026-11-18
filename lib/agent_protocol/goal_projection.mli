(** Rendering-neutral goal values. This alias adds a validating JSON-based sexp
    boundary for existing snapshot carriers; it grants no stored authority. *)
type t = Goal.t [@@deriving sexp]

(** Rejects an array exceeding sixteen before decoding any child. *)
val list_of_json : Jsonaf.t -> (t list, Error.t) result

(** At most sixteen retained identities and one enabled foreground goal, all
    owned by the supplied session with no future adopted source generation. *)
val validate
  :  t list
  -> session_id:Id.Session.t
  -> generation:int
  -> (unit, Error.t) result
