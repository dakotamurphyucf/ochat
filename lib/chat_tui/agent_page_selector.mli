(** [render ~width model] renders all current Agent and shell-script call tabs,
    wrapping complete tabs across as many rows as required by [width]. *)
val render : width:int -> Model.t -> Notty.I.t

(** [height ~width model] returns the rendered selector height. *)
val height : width:int -> Model.t -> int
