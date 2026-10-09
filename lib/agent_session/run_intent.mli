(** Durable run action associated with its actual committed handler receipt.
    Continue is consumed once at existing actual scheduling admission. Wait is
    consumed only by its exact retained wake. No independent scheduler here. *)
module Disposition : sig
  type t =
    | Pending
    | Consumed of Agent_protocol.Id.Operation.t option
    | Retired
  [@@deriving equal, sexp]
end

type t = private
  { receipt : Agent_protocol.Run_receipt.t
  ; execution_id : Agent_protocol.Id.Moderator_execution.t
  ; action : Agent_protocol.Run_action.t
  ; disposition : Disposition.t
  }
[@@deriving equal, sexp]

val create
  :  receipt:Agent_protocol.Run_receipt.t
  -> execution_id:Agent_protocol.Id.Moderator_execution.t
  -> action:Agent_protocol.Run_action.t
  -> (t, Agent_protocol.Error.t) result

val consume
  :  t
  -> operation_id:Agent_protocol.Id.Operation.t option
  -> (t, Agent_protocol.Error.t) result

val retire : t -> t
val to_jsonaf : t -> Jsonaf.t
val of_jsonaf : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t
