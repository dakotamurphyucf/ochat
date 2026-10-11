(** Result policy for sequential array tasks; neither mode starts parallel work. *)
type t =
  | Map
  | Iter

val name : t -> string
