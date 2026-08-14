type t =
  { header_height : int
  ; selector_height : int
  ; footer_height : int
  ; scroll_height : int
  }

(** [compute ~screen_w ~screen_h ~model] returns Agent-page region heights.
    Selector height expands to wrap every current call tab at [screen_w].
    Every height is non-negative and their sum does not exceed [screen_h]. *)
val compute : screen_w:int -> screen_h:int -> model:Model.t -> t
