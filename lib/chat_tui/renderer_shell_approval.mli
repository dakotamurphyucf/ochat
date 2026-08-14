(** Approval interaction overlay rendered above any active page. *)

val overlay : size:int * int -> model:Model.t -> Notty.I.t -> Notty.I.t

(** [cursor ~size ~model] returns the modal-owned text cursor when the active
    interaction contains an editor. *)
val cursor : size:int * int -> model:Model.t -> (int * int) option
