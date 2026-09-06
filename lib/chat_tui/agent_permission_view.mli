(** Shared permission presentation used by the terminal controller and trace
    tests. Repeated projections preserve the modal's client-local selection. *)
val choice_label : Agent_protocol.Permission.choice -> string

val sync
  :  Model.t
  -> current:Agent_protocol.Permission.t option
  -> Agent_projection.t
  -> Agent_protocol.Permission.t option
