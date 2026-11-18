(** Bounded current-installation lifecycle DATA. This pointer grants neither
    executable ownership nor admission; outcome belongs to its existing receipt. *)
type t [@@deriving equal, sexp]

val source : t -> Agent_protocol.Run_source.t
val execution_id : t -> Agent_protocol.Id.Moderator_execution.t

val create
  :  source:Agent_protocol.Run_source.t
  -> execution_id:Agent_protocol.Id.Moderator_execution.t
  -> (t, Agent_protocol.Error.t) result

val validate
  :  t
  -> session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> current_source:Agent_protocol.Run_source.t
  -> executions:Agent_protocol.Moderator_execution.t list
  -> (unit, Agent_protocol.Error.t) result

val lifecycle_phase : Agent_protocol.Moderator_execution.phase -> bool
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t

(** Preserve historical absence and explicit null independently. *)
module Field : sig
  type pointer = t
  type t [@@deriving equal, sexp]

  val empty : t
  val value : t -> pointer option
  val present : pointer -> t
  val clear : t -> t
  val validate_transition : previous:t -> t -> (unit, Agent_protocol.Error.t) result
  val fields : t -> (string * Jsonaf.t) list
  val of_field : Jsonaf.t option -> (t, Agent_protocol.Error.t) result
end
