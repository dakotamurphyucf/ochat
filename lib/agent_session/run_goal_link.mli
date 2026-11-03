(** Immutable association in the existing Run index. Typed/decoded facts are
    never execution authority; the actor authenticates the actual new admission. *)
type t [@@deriving equal, sexp_of]

val create
  :  goal_id:Agent_protocol.Id.Goal.t
  -> episode:Agent_protocol.Goal_episode.t
  -> intent_sha256:string
  -> (t, Agent_protocol.Error.t) result

val run_id : t -> Agent_protocol.Id.Run.t
val goal_id : t -> Agent_protocol.Id.Goal.t
val episode : t -> Agent_protocol.Goal_episode.t
val intent_sha256 : t -> string
val to_jsonaf : t -> Jsonaf.t
val of_jsonaf : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t
