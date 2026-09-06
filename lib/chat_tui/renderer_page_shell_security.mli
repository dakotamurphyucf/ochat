(** Full-screen Shell Security management page. *)

val render : size:int * int -> model:Model.t -> Notty.I.t * (int * int)
