(** Actor-owned durable source installation. Routine checkpoint writes and
    matching recovery preserve epoch; actual source replacement/removal/reset
    rotates it and retires former run scope/action custody in the same delta. *)
type t = private
  { epoch : int64
  ; source : Agent_protocol.Invocation.observer option
  }
[@@deriving equal, sexp]

module Change : sig
  type t =
    | Checkpoint of Agent_protocol.Invocation.observer option
    | Replace of Agent_protocol.Invocation.observer
    | Remove
    | Reset of Agent_protocol.Invocation.observer option
  [@@deriving sexp_of]
end

val initial : t
val apply : t -> change:Change.t -> (t, Agent_protocol.Error.t) result

val captured
  :  t
  -> generation:int
  -> (Agent_protocol.Run_source.t, Agent_protocol.Error.t) result

val to_jsonaf : t -> Jsonaf.t
val of_jsonaf : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t
