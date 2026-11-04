(** Bounded immutable per-Run association subcarrier. Historical absence/null and
    explicit arrays retain their presence; links cannot disappear or be rebound. *)
type t

val empty : t
val equal : t -> t -> bool
val entries : t -> Run_goal_link.t list
val find : t -> Agent_protocol.Id.Run.t -> Run_goal_link.t option
val add : t -> Run_goal_link.t -> (t, Agent_protocol.Error.t) result
val fields : t -> (string * Jsonaf.t) list
val of_field : Jsonaf.t option -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t
val validate_transition : previous:t -> t -> (unit, Agent_protocol.Error.t) result
